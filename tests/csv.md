Common invocation flags:

    $ function strava-gear {
    >   command strava-gear --rules rules.yaml --csv - --tablefmt csv "$@"
    > }

Rules:

    $ cat >rules.yaml <<END
    > rules:
    >   - road:
    >       chain: c1
    >   - since: 2023-01-01
    >     road:
    >       chain: c2
    >   - since: 2024-01-01
    >     road:
    >       chain: c3
    >   - '#name=Ride 3':
    >       chain: c4
    >   - '#type=VirtualRide':
    >       chain: c5
    > END

Bikes report:

    $ strava-gear <<END
    > name,gear_id,start_date,moving_time,distance,total_elevation_gain
    > Ride 1,road,2022-01-01,3600,1000,10
    > Ride 2,road,2023-01-01,3600,1000,10
    > Ride 3,road,2023-02-01,3600,1000,10
    > END
    bike,role,id,name,km,hour,first … last
    road,chain,c3,c3,0.0,0.0,never

Components report:

    $ strava-gear --report components <<END
    > name,gear_id,start_date,moving_time,distance,total_elevation_gain
    > Ride 1,road,2022-01-01,3600,1000,10
    > Ride 2,road,2023-01-01,3600,1000,10
    > Ride 3,road,2023-02-01,3600,1000,10
    > END
    id,name,km,hour,first … last
    c3,c3,0.0,0.0,never
    c5,c5,0.0,0.0,never
    c1,c1,1.0,1.0,2022-01-01 … 2022-01-01
    c2,c2,1.0,1.0,2023-01-01 … 2023-01-01
    c4,c4,1.0,1.0,2023-02-01 … 2023-02-01

VirtualRide virtual hashtag:

    $ strava-gear --report components <<END
    > name,gear_id,start_date,moving_time,distance,total_elevation_gain,type
    > Ride 1,road,2022-01-01,3600,1000,10,Ride
    > Ride 2,road,2023-01-01,3600,1000,10,Ride
    > Ride 4,road,2023-02-01,3600,1000,10,VirtualRide
    > END
    id,name,km,hour,first … last
    c3,c3,0.0,0.0,never
    c4,c4,0.0,0.0,never
    c1,c1,1.0,1.0,2022-01-01 … 2022-01-01
    c2,c2,1.0,1.0,2023-01-01 … 2023-01-01
    c5,c5,1.0,1.0,2023-02-01 … 2023-02-01

Components report imperial:

    $ strava-gear --report components --units imperial <<END
    > name,gear_id,start_date,moving_time,distance,total_elevation_gain
    > Ride 1,road,2022-01-01,3600,1000,10
    > Ride 2,road,2023-01-01,3600,1000,10
    > Ride 3,road,2023-02-01,3600,1000,10
    > END
    id,name,mi,hour,first … last
    c3,c3,0.0,0.0,never
    c5,c5,0.0,0.0,never
    c1,c1,0.621371192237334,1.0,2022-01-01 … 2022-01-01
    c2,c2,0.621371192237334,1.0,2023-01-01 … 2023-01-01
    c4,c4,0.621371192237334,1.0,2023-02-01 … 2023-02-01
