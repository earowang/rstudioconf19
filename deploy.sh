#!/bin/bash

repo=${PWD##*/}

mkdir -p ~/Web/slides/_site/$repo

# Sync folders to slides
rsync -arvzc index.html ~/Web/slides/_site/$repo/index.html
rsync -arvzc figure/* ~/Web/slides/_site/$repo/figure/
rsync -arvzc img/* ~/Web/slides/_site/$repo/img/
rsync -arvzc libs/* ~/Web/slides/_site/$repo/libs/

# Commit changes.
msg="Deploying updates to GitHub... `date`"
git add .
git commit -m "$msg"

# Push source and build repos.
git push origin master
