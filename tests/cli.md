<!--
    $ . "$TESTDIR"/.xdg-user.sh
-->

Common invocation flags:

    $ touch rules.yaml
    $ function strava-gear {
    >   command strava-gear --rules rules.yaml --tablefmt csv "$@"
    > }

Error when input doesn't exist:

    $ strava-gear
    Error: Strava database not found: /home/user/.local/share/strava_offline/strava.sqlite (run strava-gear-sync first, or use --csv or --strava-database with an existing file)
    [1]
