"""
Summarize PRs for fortran-lang within a certain month.
Requires PyGithub.
"""
import datetime
import os
from collections import namedtuple
from typing import Optional

import github


REPONAMES = ["fortran-lang.org", "stdlib", "fpm"]

CHILD_REPONAMES = {
    "stdlib": ["stdlib-cmake-example"],
    "fpm": ["setup-fpm", "fpm-haskell"],
}


def _maybe_prog(pl: github.PaginatedList.PaginatedList, desc: str):
    try:
        from tqdm import tqdm
    except ImportError:
        def tqdm(pl, *args, **kwargs):
            return pl

    return tqdm(pl, desc=desc, total=pl.totalCount, leave=False)


_Prs = namedtuple("Prs", "merged wip")


def get_prs(
    repo: github.Repository.Repository, 
    t_a: datetime.datetime,
    t_b: datetime.datetime,
):    
    merged = []  # PRs merged during the month
    pl = repo.get_pulls("closed", sort="updated", direction="desc")  # most recently *updated* first
    for pr in _maybe_prog(pl, desc=repo.name):
        if pr.merged:
            if t_a <= pr.merged_at < t_b:
                merged.append(pr)
            # if pr.merged_at < t_a:
            #     break
            # ^ This causes some to be missed since recently updated not equiv. to recently merged.
    merged.sort(key=lambda pr: pr.merged_at)  # earliest merged first

    wip = []  # WIP PRs (not merged, still open at this time)
    for pr in repo.get_pulls("open"):
        if pr.created_at < t_b: # and pr.updated_at >= t_a:
            wip.append(pr)
    
    return _Prs(merged, wip)


def main(smonth: Optional[str], token: Optional[str], skip_children: bool):
    # Time range: [t_a, t_b)
    if smonth is None:
        now = datetime.datetime.now()
        t_a = datetime.datetime(now.year, now.month, 1)
    else:
        t_a = datetime.datetime.strptime(smonth, "%Y-%m")
    t_b = datetime.datetime(t_a.year, t_a.month+1, 1)

    # GitHub connection
    if token is None:
        token = os.environ.get("GITHUB_TOKEN")
    if token is None:
        print("token not set. GitHub API might complain.")
    g = github.Github(token, per_page=100)
    org = g.get_organization("fortran-lang")

    for reponame in REPONAMES:
        print(f"\n# {reponame}\n")

        # Get main repo data
        prs = get_prs(org.get_repo(reponame), t_a, t_b)
        
        # Get child repo data
        related_prs = {
            name: get_prs(org.get_repo(name), t_a, t_b)
            for name in CHILD_REPONAMES.get(reponame, [])
        } if not skip_children else {}

        print("## merged\n")
        for pr in prs.merged:
            print(f"* [#{pr.number}]({pr.html_url}):\n  {pr.title}")
        for child_reponame, child_prs in related_prs.items():
            for pr in child_prs.merged:
                print(f"* [#{pr.number}]({pr.html_url}) (`{child_reponame}`):\n  {pr.title}")

        print("\n## WIP\n")
        for pr in prs.wip:
            print(f"* [#{pr.number}]({pr.html_url}) (WIP):\n  {pr.title}")
        for child_reponame, child_prs in related_prs.items():
            for pr in child_prs.wip:
                print(f"* [#{pr.number}]({pr.html_url}) (`{child_reponame}`; WIP):\n  {pr.title}")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        description="summarize PRs for fortran-lang"
    )
    parser.add_argument(
        "--month",
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
    parser.add_argument(
        "--skip-children",
        action="store_true",
        help="skip related repos, only fetch PRs for the main ones (fortran-lang.org, stdlib, fpm)",
        default=False,
    )
    args = parser.parse_args()

    main(args.month, args.token, args.skip_children)
