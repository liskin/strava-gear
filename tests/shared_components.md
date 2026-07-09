Common invocation flags:

    $ function strava-gear {
    >   command strava-gear --rules rules.yaml --csv - --tablefmt csv "$@"
    > }

Rules with shared component (same pedals on both bikes in the same rule):

    $ cat >rules.yaml <<END
    > rules:
    >   - road:
    >       pedals: shared-pedals
    >       chain: road-chain
    >     gravel:
    >       pedals: shared-pedals
    >       chain: gravel-chain
    > END

Bikes report - shared component appears on both bikes with combined usage:

    $ strava-gear <<END
    > name,gear_id,start_date,moving_time,distance,total_elevation_gain
    > Road Ride,road,2024-01-01,3600,10000,100
    > Gravel Ride,gravel,2024-01-02,3600,15000,200
    > END
    bike,role,id,name,km,hour,first … last
    gravel,chain,gravel-chain,gravel-chain,15.0,1.0,2024-01-02 … 2024-01-02
    gravel,pedals,shared-pedals,shared-pedals,25.0,2.0,2024-01-01 … 2024-01-02
    road,chain,road-chain,road-chain,10.0,1.0,2024-01-01 … 2024-01-01
    road,pedals,shared-pedals,shared-pedals,25.0,2.0,2024-01-01 … 2024-01-02

Components report - shared component shows total usage from both bikes:

    $ strava-gear --report components <<END
    > name,gear_id,start_date,moving_time,distance,total_elevation_gain
    > Road Ride,road,2024-01-01,3600,10000,100
    > Gravel Ride,gravel,2024-01-02,3600,15000,200
    > END
    id,name,km,hour,first … last
    road-chain,road-chain,10.0,1.0,2024-01-01 … 2024-01-01
    shared-pedals,shared-pedals,25.0,2.0,2024-01-01 … 2024-01-02
    gravel-chain,gravel-chain,15.0,1.0,2024-01-02 … 2024-01-02

Stop sharing - remove from one bike with null:

    $ cat >rules.yaml <<END
    > rules:
    >   - road:
    >       pedals: shared-pedals
    >     gravel:
    >       pedals: shared-pedals
    >   - since: 2024-06-01
    >     gravel:
    >       pedals: null
    > END

    $ strava-gear <<END
    > name,gear_id,start_date,moving_time,distance,total_elevation_gain
    > Road Ride 1,road,2024-01-01,3600,10000,100
    > Gravel Ride 1,gravel,2024-01-02,3600,15000,200
    > Road Ride 2,road,2024-07-01,3600,20000,300
    > Gravel Ride 2,gravel,2024-07-02,3600,25000,400
    > END
    bike,role,id,name,km,hour,first … last
    road,pedals,shared-pedals,shared-pedals,45.0,3.0,2024-01-01 … 2024-07-01

Moving component (different rules) still works as before:

    $ cat >rules.yaml <<END
    > rules:
    >   - road:
    >       pedals: the-pedals
    >   - since: 2024-06-01
    >     gravel:
    >       pedals: the-pedals
    > END

    $ strava-gear <<END
    > name,gear_id,start_date,moving_time,distance,total_elevation_gain
    > Road Ride,road,2024-01-01,3600,10000,100
    > Gravel Ride,gravel,2024-07-01,3600,15000,200
    > END
    bike,role,id,name,km,hour,first … last
    gravel,pedals,the-pedals,the-pedals,25.0,2.0,2024-01-01 … 2024-07-01

