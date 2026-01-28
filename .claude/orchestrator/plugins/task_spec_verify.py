"""
Task Plugin: Spec Verification

Runs e2e-test-debug-loop against spec verification items.
Priority: high (on-demand, triggered by user)
"""

from dataclasses import dataclass
from typing import Any

@dataclass
class ResourceClaim:
    screen_focus: bool = False
    file_locks: list = None
    gpu_slot: bool = False

    def __post_init__(self):
        if self.file_locks is None:
            self.file_locks = []

class TaskSpecVerify:
    name = "spec_verify"
    priority = 100  # High priority
    is_perpetual = False

    resource_requirements = ResourceClaim(
        screen_focus=True,  # Needs X11 for e2e tests
        file_locks=[],
        gpu_slot=False
    )

    def __init__(self, spec_dir: str = None, story_file: str = None):
        self.spec_dir = spec_dir or "~/vault/specs/2026-01-28-emacs-workstation-refactor"
        self.story_file = story_file

    def get_prompt(self, context: dict) -> str:
        """Generate the prompt for verification task."""
        if self.story_file:
            return f"/e2e-test {self.story_file}"

        return f"""Run verification tests for specs in {self.spec_dir}.

Use the e2e-test-debug-loop skill to:
1. Load story files from the spec directory
2. Execute each story via X11 emulated input
3. Capture screenshots and verify expectations
4. Report results

If any tests fail, analyze the failure and determine:
- Is it a genuine bug? (needs code fix)
- Is it a flaky test? (retry)
- Is it infrastructure missing? (mark blocked)

Continue until all tests pass or stagnate."""

    def check_completion(self, result: str) -> bool:
        """Return True if verification is complete."""
        # Look for completion markers
        if "ALL E2E STORIES PASS OR BLOCKED" in result:
            return True
        if "100% passed" in result.lower():
            return True
        return False

    def get_next_iteration_context(self, history: list) -> dict:
        """Context for next iteration if not complete."""
        # Extract failed stories from history
        failed_stories = []
        for entry in history:
            if "FAIL" in entry.get("result", ""):
                failed_stories.append(entry)

        return {
            "previous_failures": failed_stories,
            "iteration": len(history) + 1
        }

# Export
PLUGIN = TaskSpecVerify
