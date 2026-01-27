---
name: fixed-delay-audit
description: Audit code for fixed-delay anti-patterns. Use when the user asks to find sleep/wait/delay calls that should be condition-based, or asks about timing anti-patterns in code.
---

Check for fixed-delay anti-patterns in $ARGUMENTS.

Search code for "fixed delay" anti-patterns: any time-based wait (sleep, delay, setTimeout, time.sleep, Thread.sleep, Task.Delay, usleep, nanosleep, wait_ms, etc.) that is not gated by a condition check. Every wait should have an exit condition that isn't just elapsed time. Valid: loops that poll a predicate (while !ready: sleep()), event subscriptions, callbacks, signals, futexes, condition variables, semaphores, promise/await on actual events. Invalid: sequential sleep(N); do_thing() assuming N is sufficient, or do_thing(); sleep(N) hoping state settles. For each occurrence, determine: what state is this wait actually waiting for? If answerable (process exit, resource availability, service ready, lock release, file existence, socket open, queue non-empty), replace with condition-based polling or event-driven mechanism. If unanswerable, it's likely legitimate UX timing (animation, debounce, rate-limit, human perception delay) - mark as intentional. Report: location, current code, inferred wait condition, and suggested event-driven or condition-polled alternative.
