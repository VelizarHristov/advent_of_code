# Code generated by ChatGPT o1-preview - on first try according to a friend
# For day 2, part 1 - solves it successfully
# I did not use this to solve the problem, it is just here because it's interesting
# To run on Windows with PowerShell:
# Get-Content resources\2024 | python.exe .\gpt.py

def is_safe_report(levels):
    differences = []
    for i in range(len(levels) - 1):
        diff = levels[i+1] - levels[i]
        if diff == 0:
            return False  # Difference cannot be zero
        differences.append(diff)

    # Check if all differences are positive (increasing) or all negative (decreasing)
    all_increasing = all(d > 0 for d in differences)
    all_decreasing = all(d < 0 for d in differences)

    if not (all_increasing or all_decreasing):
        return False

    # Check if all absolute differences are between 1 and 3
    if not all(1 <= abs(d) <= 3 for d in differences):
        return False

    return True

def main():
    safe_reports = 0
    try:
        while True:
            line = input()
            if not line.strip():
                continue  # Skip empty lines
            levels = list(map(int, line.strip().split()))
            if is_safe_report(levels):
                safe_reports += 1
    except EOFError:
        pass  # End of input
    print(safe_reports)

if __name__ == "__main__":
    main()