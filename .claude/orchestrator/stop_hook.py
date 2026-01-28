#!/usr/bin/env python3
"""
Claude Orchestrator Stop Hook

Thin client that queries the orchestrator daemon to determine
whether a Claude session should be allowed to exit.

Called by Claude Code's Stop hook system via hooks.json.
Outputs JSON to stdout: {"decision": "allow"|"block", "reason": "..."}

Exit codes:
  0 = decision made (check stdout)
  1 = daemon not running or error (allow exit by default)
"""

import json
import os
import socket
import sys
from pathlib import Path

SOCKET_PATH = Path.home() / ".claude" / "orchestrator" / "daemon.sock"
TIMEOUT_SEC = 0.1  # 100ms max latency

def query_daemon(session_id: str) -> dict:
    """Query the orchestrator daemon via Unix socket."""
    if not SOCKET_PATH.exists():
        return {"decision": "allow", "reason": "daemon not running"}

    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.settimeout(TIMEOUT_SEC)
        sock.connect(str(SOCKET_PATH))

        request = json.dumps({
            "method": "should_continue",
            "params": {"session_id": session_id}
        })
        sock.sendall(request.encode())

        response = sock.recv(65536)
        sock.close()

        data = json.loads(response.decode())
        return data.get("result", {"decision": "allow", "reason": "no result"})

    except socket.timeout:
        return {"decision": "allow", "reason": "daemon timeout"}
    except ConnectionRefusedError:
        return {"decision": "allow", "reason": "daemon not running"}
    except Exception as e:
        return {"decision": "allow", "reason": f"error: {e}"}

def main():
    # Get session ID from environment or generate from PID
    session_id = os.environ.get("CLAUDE_SESSION_ID", f"session-{os.getppid()}")

    result = query_daemon(session_id)

    # Output JSON for Claude Code hook system
    print(json.dumps(result))

    # Exit 0 regardless - the hook system reads stdout
    sys.exit(0)

if __name__ == "__main__":
    main()
