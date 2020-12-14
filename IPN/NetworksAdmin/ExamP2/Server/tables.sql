create table router (
    name varchar(30) not null primary key,
    ip_id varchar(30) not null,
    model varchar(30) not null,
    version varchar(30) not null
);

create table user (
    username varchar(30) not null primary key,
    router_name varchar(30) not null,
    password varchar(30) not null
);

create table interface (
    name varchar(30) not null,
    router_name varchar(30) not null,

    ip_id varchar(30) not null,
    mask varchar(30) not null,
    connected_router_name varchar(30)
);


