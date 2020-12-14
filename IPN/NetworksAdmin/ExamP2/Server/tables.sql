create table router (
    router_id integer auto_increment primary key,
    name varchar(20) not null unique,
    ip_id varchar(20) not null,
    model varchar(20) not null,
    os varchar(20) not null
);

create table user (
    username varchar(20) not null primary key,
    router_id integer not null,
    password varchar(20) not null,
    foreign key (router_id)
        references router(router_id)
        on delete cascade
);

create table interface (
    name varchar(20) not null unique primary key,
    router_id integer not null,
    ip varchar(20) not null,
    mask varchar(20) not null,
    connected_router_id integer,

    foreign key (router_id)
        references router(router_id)
        on delete cascade,

    foreign key (connected_router_id)
        references router(router_id)
        on delete set null
);

insert into router (name, ip_id, model, os) values ('R1', '10.10.10.1', 'CISCO', 'v10');