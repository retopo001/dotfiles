#!/usr/bin/env python3
"""
Orchestrator Client

Simple client for interacting with the orchestrator daemon.

Usage:
    python client.py status
    python client.py register <session_id> [capabilities...]
    python client.py heartbeat <session_id>
    python client.py submit <submitter> <payload_json> [--priority N]
    python client.py claim <session_id>
    python client.py complete <session_id> <task_id>
    python client.py fail <session_id> <task_id> <error>
"""

import argparse
import json
import socket
import sys
from pathlib import Path

SOCKET_PATH = Path.home() / ".claude" / "orchestrator" / "daemon.sock"

def send_request(method: str, params: dict) -> dict:
    """Send a request to the orchestrator daemon."""
    if not SOCKET_PATH.exists():
        return {"error": "daemon not running"}

    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.settimeout(5.0)
        sock.connect(str(SOCKET_PATH))

        request = json.dumps({"method": method, "params": params})
        sock.sendall(request.encode())

        response = sock.recv(65536)
        sock.close()

        return json.loads(response.decode())

    except Exception as e:
        return {"error": str(e)}

def main():
    parser = argparse.ArgumentParser(description="Orchestrator Client")
    subparsers = parser.add_subparsers(dest="command", required=True)

    # status
    subparsers.add_parser("status", help="Get orchestrator status")

    # register
    reg = subparsers.add_parser("register", help="Register a session")
    reg.add_argument("session_id")
    reg.add_argument("capabilities", nargs="*", default=[])

    # heartbeat
    hb = subparsers.add_parser("heartbeat", help="Send heartbeat")
    hb.add_argument("session_id")

    # unregister
    unreg = subparsers.add_parser("unregister", help="Unregister a session")
    unreg.add_argument("session_id")

    # submit
    sub = subparsers.add_parser("submit", help="Submit a task")
    sub.add_argument("submitter")
    sub.add_argument("payload", help="JSON payload")
    sub.add_argument("--priority", type=int, default=0)
    sub.add_argument("--capabilities", nargs="*", default=[])
    sub.add_argument("--type", default="one_shot", choices=["one_shot", "recurring", "perpetual"])

    # claim
    claim = subparsers.add_parser("claim", help="Claim a task")
    claim.add_argument("session_id")

    # complete
    comp = subparsers.add_parser("complete", help="Complete a task")
    comp.add_argument("session_id")
    comp.add_argument("task_id")

    # fail
    fail = subparsers.add_parser("fail", help="Fail a task")
    fail.add_argument("session_id")
    fail.add_argument("task_id")
    fail.add_argument("error")

    # get_task
    gt = subparsers.add_parser("task", help="Get task status")
    gt.add_argument("task_id")

    args = parser.parse_args()

    if args.command == "status":
        result = send_request("get_status", {})
    elif args.command == "register":
        result = send_request("register_session", {
            "session_id": args.session_id,
            "capabilities": args.capabilities
        })
    elif args.command == "heartbeat":
        result = send_request("heartbeat", {"session_id": args.session_id})
    elif args.command == "unregister":
        result = send_request("unregister_session", {"session_id": args.session_id})
    elif args.command == "submit":
        result = send_request("submit_task", {
            "submitter_id": args.submitter,
            "payload": json.loads(args.payload),
            "priority": args.priority,
            "required_capabilities": args.capabilities,
            "type": args.type
        })
    elif args.command == "claim":
        result = send_request("claim_task", {"session_id": args.session_id})
    elif args.command == "complete":
        result = send_request("complete_task", {
            "session_id": args.session_id,
            "task_id": args.task_id,
            "result": {}
        })
    elif args.command == "fail":
        result = send_request("fail_task", {
            "session_id": args.session_id,
            "task_id": args.task_id,
            "error": args.error
        })
    elif args.command == "task":
        result = send_request("get_task", {"task_id": args.task_id})
    else:
        result = {"error": "unknown command"}

    print(json.dumps(result, indent=2))

if __name__ == "__main__":
    main()
