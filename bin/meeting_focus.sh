#!/bin/bash

# Toggle macOS Focus based on whether a calendar event is happening now.
# Requires:
#   - icalBuddy with Calendar permission (System Settings -> Privacy -> Calendars)
#   - macos-focus-mode CLI (https://github.com/arodik/macos-focus-mode),
#     which wraps the "macos-focus-mode" Shortcut.

MEETING=$(/opt/homebrew/bin/icalbuddy -ea -ic "ilja.kocken@fwdfaster.ai" eventsNow)

if [ -n "$MEETING" ]; then
    /opt/homebrew/bin/macos-focus-mode enable --silent
else
    /opt/homebrew/bin/macos-focus-mode disable --silent
fi
