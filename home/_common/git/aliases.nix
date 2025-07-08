{...}: {
  programs.git.aliases = {
    aliases = "config --get-regexp alias";
    ac = "add -i && git commit -avm";
    am = "commit --amend --no-edit";
    b = "branch -a";
    c = "commit -avm";
    cm = "commit -m";
    d = "! git diff-index --quiet HEAD -- || clear; git --no-pager diff --patch-with-stat";
    ex = "update-index --assume-unchanged";
    nex = "update-index --no-assume-unchanged";
    fu = "fetch upstream";
    l = "log --pretty=oneline --abbrev-commit -12";
    ll = "log --pretty=oneline --abbrev-commit";
    r = "remote -v";
    rau = "remote add upstream";
    s = "status -s";
    undo = "reset HEAD~";
    ds = "-c delta.features=side-by-side diff";
  };
}
