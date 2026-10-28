# Local Supabase stacks, for every repo with one. A repo's justfile pulls them in with
#   import? '~/ArchConfigs/agents/just/db.just'
# A stack is a tracked `<dir>/supabase/config.toml`; its `project_id` names its containers. Start one
# when a task needs it; an idle one stops on its own (~/ArchConfigs/bin/stacks-idle-stop, via
# launchd), so `db-down` is for freeing memory now.

# start this repo's Supabase stacks (all, or the dirs given), after listing what already runs
db-up *dirs:
    #!/usr/bin/env bash
    set -euo pipefail
    docker info >/dev/null 2>&1 || { echo "Docker isn't running: start Docker Desktop first"; exit 1; }
    running=$(docker ps --format '{{{{.Names}}' | sed -n 's/^supabase_db_//p' | sort)
    echo "running on this machine (all repos): $(echo ${running:-none})"
    for dir in {{ if dirs == "" { "$(git ls-files '*supabase/config.toml' | xargs -n1 dirname | xargs -n1 dirname)" } else { dirs } }}; do
        project=$(sed -n 's/^project_id = "\(.*\)"/\1/p' "$dir/supabase/config.toml")
        if grep -qx "$project" <<<"$running"; then echo "$project: up"; continue; fi
        echo "$project: starting (first start applies every migration, a minute or more)"
        log=$(mktemp)
        (cd "$dir" && supabase start >"$log" 2>&1) || { cat "$log"; exit 1; }
        echo "$project: up"
    done

# stop this repo's Supabase stacks (all, or the dirs given); data is kept
db-down *dirs:
    #!/usr/bin/env bash
    set -euo pipefail
    for dir in {{ if dirs == "" { "$(git ls-files '*supabase/config.toml' | xargs -n1 dirname | xargs -n1 dirname)" } else { dirs } }}; do
        project=$(sed -n 's/^project_id = "\(.*\)"/\1/p' "$dir/supabase/config.toml")
        supabase stop --project-id "$project" >/dev/null && echo "$project: stopped"
    done
