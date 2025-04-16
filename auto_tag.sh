#!/bin/zsh

# Get the latest tag
LATEST_TAG=$(git describe --tags --abbrev=0)

# Extract version components (assuming semantic versioning)
MAJOR=$(echo "$LATEST_TAG" | cut -d. -f1)
MINOR=$(echo "$LATEST_TAG" | cut -d. -f2)
PATCH=$(echo "$LATEST_TAG" | cut -d. -f3)

# Increment PATCH version
NEW_TAG="$MAJOR.$MINOR.$((PATCH + 1))"

# Create and push the new tag
git tag -a "$NEW_TAG" -m "Automated release version $NEW_TAG"
git push origin "$NEW_TAG"

echo "Created and pushed tag: $NEW_TAG"