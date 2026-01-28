#!/usr/bin/env python3
"""
Coordinator Auto-Registration Hook

Runs on SessionStart to register with the orchestrator.
Runs on Stop to check if session should continue or exit.

Supports both:
- Python orchestrator (preferred, via Unix socket)
- Go MCP coordinator (fallback, via stdio)
"""

import json
import os
import socket
import subprocess
import sys
from pathlib import Path

# Python orchestrator
ORCHESTRATOR_SOCKET = Path.home() / ".claude" / "orchestrator" / "daemon.sock"
SESSION_FILE = Path.home() / ".claude" / "orchestrator" / "current_session.json"

# Go coordinator (fallback)
COORDINATOR_BIN = Path.home() / "vault" / "programs" / "claude-coordinator" / "bin" / "coordinator"

def call_orchestrator(method: str, params: dict) -> dict:
    """Call Python orchestrator via Unix socket."""
    if not ORCHESTRATOR_SOCKET.exists():
        return None  # Fallback to Go coordinator

    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.settimeout(0.1)  # 100ms
        sock.connect(str(ORCHESTRATOR_SOCKET))

        request = json.dumps({"method": method, "params": params})
        sock.sendall(request.encode())

        response = sock.recv(65536)
        sock.close()

        data = json.loads(response.decode())
        return data.get("result", data)
    except Exception as e:
        return None

def call_go_coordinator(method: str, params: dict) -> dict:
    """Call Go coordinator via MCP protocol (fallback)."""
    if not COORDINATOR_BIN.exists():
        return {"error": "Coordinator not found"}

    try:
        proc = subprocess.Popen(
            [str(COORDINATOR_BIN)],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )

        # Initialize
        init_req = json.dumps({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {"name": "coordinator-hook", "version": "0.1.0"}
            }
        }) + "\n"
        proc.stdin.write(init_req)
        proc.stdin.flush()
        proc.stdout.readline()

        proc.stdin.write(json.dumps({"jsonrpc": "2.0", "method": "notifications/initialized", "params": {}}) + "\n")
        proc.stdin.flush()

        tool_req = json.dumps({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "tools/call",
            "params": {"name": method, "arguments": params}
        }) + "\n"
        proc.stdin.write(tool_req)
        proc.stdin.flush()

        response = proc.stdout.readline()
        proc.terminate()

        if response:
            result = json.loads(response)
            content = result.get("result", {}).get("content", [])
            if content and content[0].get("type") == "text":
                try:
                    return json.loads(content[0].get("text", "{}"))
                except:
                    return {"message": content[0].get("text", "")}
        return {}
    except Exception:
        return {"error": "Go coordinator call failed"}

def call_coordinator(method: str, params: dict) -> dict:
    """Call coordinator - Python orchestrator first, then Go fallback."""
    result = call_orchestrator(method, params)
    if result is not None:
        return result
    return call_go_coordinator(method, params)

def get_capabilities() -> list:
    """Determine session capabilities based on environment."""
    capabilities = ["filesystem", "bash"]

    # Check if chrome extension is likely available
    if os.environ.get("CLAUDE_IN_CHROME_ENABLED", "") or Path.home().joinpath(".claude.json").exists():
        capabilities.append("chrome-browser")

    # Add x11 if DISPLAY is set
    if os.environ.get("DISPLAY"):
        capabilities.append("x11")

    return capabilities

def register_session():
    """Register this session with the orchestrator."""
    capabilities = get_capabilities()
    session_id = f"interactive-{os.getpid()}"

    result = call_coordinator("register_session", {
        "session_id": session_id,
        "name": session_id,
        "capabilities": capabilities
    })

    if result and "session_id" in result:
        SESSION_FILE.parent.mkdir(parents=True, exist_ok=True)
        SESSION_FILE.write_text(json.dumps({
            "session_id": result["session_id"],
            "capabilities": capabilities,
            "pid": os.getpid()
        }))
        print(f"[orchestrator] Registered: {result['session_id']}", file=sys.stderr)
        return result["session_id"]
    elif result and "status" in result:
        # Python orchestrator response
        SESSION_FILE.parent.mkdir(parents=True, exist_ok=True)
        SESSION_FILE.write_text(json.dumps({
            "session_id": session_id,
            "capabilities": capabilities,
            "pid": os.getpid()
        }))
        print(f"[orchestrator] Registered: {session_id}", file=sys.stderr)
        return session_id
    else:
        print(f"[orchestrator] Registration failed: {result}", file=sys.stderr)
        return None

def check_should_continue() -> dict:
    """Check if session should continue or exit."""
    if not SESSION_FILE.exists():
        return {"decision": "allow", "reason": "no session registered"}

    try:
        session_data = json.loads(SESSION_FILE.read_text())
        session_id = session_data.get("session_id")
    except Exception:
        return {"decision": "allow", "reason": "invalid session file"}

    if not session_id:
        return {"decision": "allow", "reason": "no session_id"}

    # Check with orchestrator
    result = call_orchestrator("should_continue", {"session_id": session_id})
    if result and "decision" in result:
        return result

    # Fallback: just check pending tasks via heartbeat
    result = call_coordinator("heartbeat", {"session_id": session_id})
    pending = result.get("pending_tasks", 0) if result else 0

    if pending > 0:
        return {"decision": "block", "reason": f"{pending} pending task(s) available"}

    return {"decision": "allow", "reason": "no pending work"}

def on_stop():
    """Called when session is about to stop."""
    result = check_should_continue()

    if result.get("decision") == "block":
        # Output the decision for Claude Code hook system
        print(json.dumps(result))
    else:
        # Allow exit
        pass

def main():
    hook_type = os.environ.get("CLAUDE_HOOK_TYPE", "")

    if hook_type == "SessionStart":
        register_session()
    elif hook_type == "Stop":
        on_stop()
    else:
        on_stop()

if __name__ == "__main__":
    main()
