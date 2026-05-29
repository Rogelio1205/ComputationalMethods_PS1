"""
Task Manager - Command-line task management tool
Supports creating, listing, updating, deleting, and filtering tasks
with priorities, tags, and due dates.
"""

import json
import os
import sys
from datetime import datetime, date
from typing import Optional


# ─────────────────────────────────────────────
#  Configuration
# ─────────────────────────────────────────────

DATA_FILE = "tasks.json"
DATE_FMT  = "%Y-%m-%d"

PRIORITIES = {"low": 1, "medium": 2, "high": 3, "critical": 4}
COLORS = {
    "reset":  "\033[0m",
    "bold":   "\033[1m",
    "red":    "\033[91m",
    "green":  "\033[92m",
    "yellow": "\033[93m",
    "blue":   "\033[94m",
    "cyan":   "\033[96m",
    "gray":   "\033[90m",
}

PRIORITY_COLORS = {
    "low":      COLORS["gray"],
    "medium":   COLORS["blue"],
    "high":     COLORS["yellow"],
    "critical": COLORS["red"],
}


def c(color: str, text: str) -> str:
    """Apply ANSI color to text."""
    return f"{COLORS.get(color, '')}{text}{COLORS['reset']}"


# ─────────────────────────────────────────────
#  Data model
# ─────────────────────────────────────────────

def new_task(title: str, priority: str = "medium",
             due: Optional[str] = None, tags: Optional[list] = None,
             task_id: Optional[int] = None) -> dict:
    """Create a dictionary representing a task."""
    return {
        "id":         task_id,
        "title":      title.strip(),
        "priority":   priority.lower(),
        "done":       False,
        "due":        due,
        "tags":       tags or [],
        "created_at": datetime.now().strftime("%Y-%m-%d %H:%M"),
    }


# ─────────────────────────────────────────────
#  Persistence
# ─────────────────────────────────────────────

def load_tasks() -> list:
    """Load tasks from the JSON file."""
    if not os.path.exists(DATA_FILE):
        return []
    with open(DATA_FILE, "r", encoding="utf-8") as f:
        return json.load(f)


def save_tasks(tasks: list) -> None:
    """Save the task list to the JSON file."""
    with open(DATA_FILE, "w", encoding="utf-8") as f:
        json.dump(tasks, f, ensure_ascii=False, indent=2)


def next_id(tasks: list) -> int:
    """Generate the next available ID."""
    return max((t["id"] for t in tasks), default=0) + 1


# ─────────────────────────────────────────────
#  CRUD operations
# ─────────────────────────────────────────────

def add_task(title: str, priority: str = "medium",
             due: Optional[str] = None, tags: Optional[list] = None) -> dict:
    """Add a new task."""
    if priority not in PRIORITIES:
        raise ValueError(f"Invalid priority: '{priority}'. Use: {list(PRIORITIES)}")
    if due:
        try:
            datetime.strptime(due, DATE_FMT)
        except ValueError:
            raise ValueError(f"Invalid date: '{due}'. Use YYYY-MM-DD format.")

    tasks = load_tasks()
    task = new_task(title, priority, due, tags, task_id=next_id(tasks))
    tasks.append(task)
    save_tasks(tasks)
    return task


def get_task(task_id: int) -> Optional[dict]:
    """Retrieve a task by its ID."""
    return next((t for t in load_tasks() if t["id"] == task_id), None)


def update_task(task_id: int, **kwargs) -> Optional[dict]:
    """Update fields of an existing task."""
    tasks = load_tasks()
    for task in tasks:
        if task["id"] == task_id:
            for key, value in kwargs.items():
                if key in task:
                    task[key] = value
            save_tasks(tasks)
            return task
    return None


def delete_task(task_id: int) -> bool:
    """Delete a task by its ID."""
    tasks = load_tasks()
    original = len(tasks)
    tasks = [t for t in tasks if t["id"] != task_id]
    if len(tasks) < original:
        save_tasks(tasks)
        return True
    return False


def complete_task(task_id: int) -> Optional[dict]:
    """Mark a task as completed."""
    return update_task(task_id, done=True)


# ─────────────────────────────────────────────
#  Filters and search
# ─────────────────────────────────────────────

def filter_tasks(tasks: list, *, done: Optional[bool] = None,
                 priority: Optional[str] = None, tag: Optional[str] = None,
                 overdue: bool = False) -> list:
    """Filter tasks by criteria."""
    today = date.today()
    result = tasks

    if done is not None:
        result = [t for t in result if t["done"] == done]
    if priority:
        result = [t for t in result if t["priority"] == priority]
    if tag:
        result = [t for t in result if tag in t.get("tags", [])]
    if overdue:
        result = [
            t for t in result
            if t.get("due") and datetime.strptime(t["due"], DATE_FMT).date() < today and not t["done"]
        ]
    return result


def search_tasks(query: str) -> list:
    """Search tasks whose title contains the given text."""
    q = query.lower()
    return [t for t in load_tasks() if q in t["title"].lower()]


def sort_tasks(tasks: list, by: str = "priority") -> list:
    """Sort tasks. Criteria: 'priority', 'due', 'id'."""
    if by == "priority":
        return sorted(tasks, key=lambda t: PRIORITIES.get(t["priority"], 0), reverse=True)
    if by == "due":
        return sorted(tasks, key=lambda t: t.get("due") or "9999-99-99")
    return sorted(tasks, key=lambda t: t["id"])


# ─────────────────────────────────────────────
#  Display
# ─────────────────────────────────────────────

def format_due(due: Optional[str]) -> str:
    """Format the due date with color based on urgency."""
    if not due:
        return c("gray", "no date")
    due_date = datetime.strptime(due, DATE_FMT).date()
    today = date.today()
    delta = (due_date - today).days
    if delta < 0:
        return c("red", f"⚠ {due} (overdue)")
    if delta == 0:
        return c("yellow", f"🔥 {due} (today)")
    if delta <= 3:
        return c("yellow", f"⏰ {due} ({delta}d)")
    return c("green", due)


def print_task(task: dict, verbose: bool = False) -> None:
    """Print a formatted task."""
    status = c("green", "✓") if task["done"] else c("gray", "○")
    pri_color = PRIORITY_COLORS.get(task["priority"], "")
    pri_label = f"{pri_color}[{task['priority']:^8}]{COLORS['reset']}"
    title = c("gray", task["title"]) if task["done"] else c("bold", task["title"])
    tags = "  " + " ".join(c("cyan", f"#{t}") for t in task["tags"]) if task["tags"] else ""

    print(f"  {status} {c('gray', f'#{task[\"id\"]:03d}')}  {pri_label}  {title}{tags}")

    if verbose:
        print(f"       📅 Due date:  {format_due(task.get('due'))}")
        print(f"       🕐 Created:   {c('gray', task['created_at'])}")


def print_task_list(tasks: list, title: str = "Tasks", verbose: bool = False) -> None:
    """Print a task list with a header."""
    print()
    print(c("bold", f"  ── {title} ({len(tasks)}) ──"))
    print()
    if not tasks:
        print(c("gray", "  (no tasks found)"))
    else:
        for task in tasks:
            print_task(task, verbose=verbose)
    print()


def print_stats(tasks: list) -> None:
    """Display task manager statistics."""
    total   = len(tasks)
    done    = sum(1 for t in tasks if t["done"])
    pending = total - done
    overdue = len(filter_tasks(tasks, overdue=True))
    by_pri  = {p: sum(1 for t in tasks if t["priority"] == p and not t["done"])
               for p in PRIORITIES}

    print()
    print(c("bold", "  ── Statistics ──"))
    print()
    print(f"  Total:     {total}")
    print(f"  Completed: {c('green', str(done))}")
    print(f"  Pending:   {c('yellow', str(pending))}")
    print(f"  Overdue:   {c('red', str(overdue))}")
    print()
    print("  By priority (pending):")
    for p, count in by_pri.items():
        bar = "█" * count
        col = PRIORITY_COLORS.get(p, "")
        print(f"    {p:10} {col}{bar} {count}{COLORS['reset']}")
    print()


# ─────────────────────────────────────────────
#  Command-line interface
# ─────────────────────────────────────────────

HELP_TEXT = f"""
{c('bold', 'Task Manager — Available commands')}

  {c('cyan', 'add')} <title> [options]       Create a new task
    --priority, -p <level>    low | medium | high | critical  (default: medium)
    --due,      -d <date>     Due date in YYYY-MM-DD format
    --tags,     -t <tags>     Comma-separated tags

  {c('cyan', 'list')} [options]              List tasks
    --all, -a                 Include completed tasks
    --priority, -p <level>    Filter by priority
    --tag,      -t <tag>      Filter by tag
    --overdue                 Show only overdue tasks
    --sort <criteria>         id | priority | due  (default: priority)
    --verbose, -v             Show full details

  {c('cyan', 'done')} <id>                   Mark task as completed
  {c('cyan', 'delete')} <id>                 Delete a task
  {c('cyan', 'search')} <text>               Search task titles
  {c('cyan', 'stats')}                       Show statistics
  {c('cyan', 'help')}                        Show this help message
"""


def parse_args(args: list) -> tuple:
    """Simple argument parser with no external dependencies."""
    opts = {}
    positional = []
    i = 0
    while i < len(args):
        a = args[i]
        if a.startswith("--"):
            key = a[2:]
            if i + 1 < len(args) and not args[i + 1].startswith("-"):
                opts[key] = args[i + 1]; i += 2
            else:
                opts[key] = True; i += 1
        elif a.startswith("-") and len(a) == 2:
            short_map = {"p": "priority", "d": "due", "t": "tags",
                         "a": "all", "v": "verbose"}
            key = short_map.get(a[1], a[1])
            if i + 1 < len(args) and not args[i + 1].startswith("-"):
                opts[key] = args[i + 1]; i += 2
            else:
                opts[key] = True; i += 1
        else:
            positional.append(a); i += 1
    return positional, opts


def run(argv: list) -> None:
    """Main entry point."""
    if len(argv) < 2:
        print(HELP_TEXT)
        return

    cmd  = argv[1].lower()
    rest = argv[2:]
    pos, opts = parse_args(rest)

    # ── add ──────────────────────────────────
    if cmd == "add":
        if not pos:
            print(c("red", "  Error: you must provide a title.")); return
        title    = " ".join(pos)
        priority = opts.get("priority", "medium")
        due      = opts.get("due")
        tags_raw = opts.get("tags")
        tags     = [t.strip() for t in tags_raw.split(",")] if tags_raw else []
        try:
            task = add_task(title, priority, due, tags)
            print(c("green", f"\n  ✓ Task #{task['id']} created: {task['title']}\n"))
        except ValueError as e:
            print(c("red", f"\n  Error: {e}\n"))

    # ── list ─────────────────────────────────
    elif cmd == "list":
        tasks = load_tasks()
        include_done = "all" in opts
        if not include_done:
            tasks = filter_tasks(tasks, done=False)
        if "priority" in opts:
            tasks = filter_tasks(tasks, priority=opts["priority"])
        if "tag" in opts:
            tasks = filter_tasks(tasks, tag=opts["tag"])
        if "overdue" in opts:
            tasks = filter_tasks(tasks, overdue=True)
        sort_by = opts.get("sort", "priority")
        tasks = sort_tasks(tasks, by=sort_by)
        label = "All tasks" if include_done else "Pending tasks"
        print_task_list(tasks, title=label, verbose="verbose" in opts)

    # ── done ─────────────────────────────────
    elif cmd == "done":
        if not pos:
            print(c("red", "  Error: provide the task ID.")); return
        task = complete_task(int(pos[0]))
        if task:
            print(c("green", f"\n  ✓ Task #{task['id']} marked as completed.\n"))
        else:
            print(c("red", f"\n  Task #{pos[0]} not found.\n"))

    # ── delete ───────────────────────────────
    elif cmd == "delete":
        if not pos:
            print(c("red", "  Error: provide the task ID.")); return
        if delete_task(int(pos[0])):
            print(c("yellow", f"\n  🗑  Task #{pos[0]} deleted.\n"))
        else:
            print(c("red", f"\n  Task #{pos[0]} not found.\n"))

    # ── search ───────────────────────────────
    elif cmd == "search":
        if not pos:
            print(c("red", "  Error: provide a search query.")); return
        results = search_tasks(" ".join(pos))
        print_task_list(results, title=f'Results for "{" ".join(pos)}"')

    # ── stats ────────────────────────────────
    elif cmd == "stats":
        print_stats(load_tasks())

    # ── help ─────────────────────────────────
    elif cmd in ("help", "--help", "-h"):
        print(HELP_TEXT)

    else:
        print(c("red", f"\n  Unknown command: '{cmd}'. Use 'help' to see available commands.\n"))


# ─────────────────────────────────────────────
#  Entry point
# ─────────────────────────────────────────────

if __name__ == "__main__":
    run(sys.argv)