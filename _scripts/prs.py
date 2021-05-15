"""
Summarize PRs for fortran-lang within a certain month.
Requires PyGithub.
"""
import datetime
import os
from typing import Optional

import github


def get_prs(
    repo: github.Repository.Repository, 
    t_a: datetime.datetime,
    t_b: datetime.datetime,
):    
    merged = []  # PRs merged during the month
    for pr in repo.get_pulls("closed", sort="updated", direction="desc"):  # most recently merged first
        if pr.merged:
            if t_a <= pr.merged_at < t_b:  # could check `.month` instead...
                merged.append(pr)
            if pr.merged_at < t_a:
                break
            
    wip = []  # WIP PRs (not merged, still open at this time)
    for pr in repo.get_pulls("open"):
        if pr.created_at < t_b: # and pr.updated_at >= t_a:
            wip.append(pr)
    
    return merged, wip


def main(sdate: Optional[str], token: Optional[str]):
    # Time range: [t_a, t_b)
    if sdate is None:
        now = datetime.datetime.now()
        t_a = datetime.datetime(now.year, now.month, 1)
    else:
        t_a = datetime.datetime.strptime(sdate, "%Y-%m")
    t_b = datetime.datetime(t_a.year, t_a.month+1, 1)

    # GitHub connection
    if token is None:
        token = os.environ.get("GITHUB_TOKEN")
    if token is None:
        print("token not set. GitHub API may complain")
    g = github.Github(token, per_page=100)
    org = g.get_organization("fortran-lang")

    for reponame in ["stdlib", "fpm"]:
        print(f"\n# {reponame}\n")
        merged, wip = get_prs(org.get_repo(reponame), t_a, t_b)
        print("## merged\n")
        for pr in merged:
            print(f"* [#{pr.number}]({pr.html_url}): {pr.title}")
        print("\n## WIP\n")
        for pr in wip:
            print(f"* [#{pr.number}]({pr.html_url}) (WIP): {pr.title}")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="summarize PRs for fortran-lang"
    )
    parser.add_argument(
        "--date",
        action="store",
        help="month in YYYY-MM format (current if not specified)",
        default=None,
    )
    parser.add_argument(
        "--token",
        action="store",
        help="GitHub access token (overrides GITHUB_TOKEN env var if that is also set)",
        default=None,
    )
    args = parser.parse_args()

    main(args.date, args.token)
