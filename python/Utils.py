from typing import Callable, Iterable, List, Optional, TypeVar


T = TypeVar("T")


def rotate(n: int, items: List[T]) -> List[T]:
    if not items:
        raise ValueError("empty list")
    if n == 0:
        return items
    return rotate(n - 1, items[1:] + [items[0]])


def without(indices: List[int], items: List[T]) -> List[T]:
    return applyAtMaybe(indices, lambda _e: None, items)


def applyAt(indices: List[int], fn: Callable[[T], T], items: List[T]) -> List[T]:
    return applyAtMaybe(indices, lambda e: fn(e), items)


def applyAtMaybe(
    indices: List[int], fn: Callable[[T], Optional[T]], items: List[T]
) -> List[T]:
    if not indices:
        return items
    if not items:
        return []

    head, tail = items[0], items[1:]
    decremented = [i - 1 for i in indices]
    if indices[0] == 0:
        mapped = fn(head)
        rest = applyAtMaybe(decremented[1:], fn, tail)
        if mapped is None:
            return rest
        return [mapped] + rest
    return [head] + applyAtMaybe(decremented, fn, tail)


def putLines(show_fn: Callable[[T], str], lines: Iterable[T]) -> None:
    for line in lines:
        print(show_fn(line))


def subscript(x: int) -> str:
    mapping = {
        "0": "₀",
        "1": "₁",
        "2": "₂",
        "3": "₃",
        "4": "₄",
        "5": "₅",
        "6": "₆",
        "7": "₇",
        "8": "₈",
        "9": "₉",
    }
    return "".join(mapping.get(ch, ch) for ch in str(x))
