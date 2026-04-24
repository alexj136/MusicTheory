from dataclasses import dataclass
from typing import Callable, List


@dataclass
class Test:
    failMsg: str
    runTest: bool


def testEqual(msg: str, expected, actual) -> Test:
    elaborate = f"{msg}\n    Expected: {expected}\n    Actual: {actual}"
    return Test(elaborate, expected == actual)


def testMain(tests: List[Test]) -> None:
    total = 0
    passed = 0
    for test in tests:
        total += 1
        if test.runTest:
            passed += 1
        else:
            print(f"FAIL: {test.failMsg}")
    print(f"\nTests run: {passed} passed / {total - passed} failed / {total} total. ")
