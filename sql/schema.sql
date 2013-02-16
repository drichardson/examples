-- People and Relationships
create table person (
	id serial primary key,
	name varchar(100) not null,
	birthdate date not null);

create table alsoKnownAs(
	personId serial not null,
	otherName varchar(100) not null,
	unique(personId, otherName));

create table connection(
	person1Id serial not null,
	person2Id serial not null,
	unique(person1Id, person2Id));

insert into person(name, birthdate) values ('Douglas Richardson', 'May 2, 1979');
insert into person(name, birthdate) values ('Rebecca Richardson', 'November 29, 1979');
insert into person(name, birthdate) values ('Matthew Mitchel', 'Aug 2, 1976');
insert into person(name, birthdate) values ('Kirk Sheby', 'June 2, 1983');
insert into person(name, birthdate) values ('Amber Beltran', 'May 2, 1979');

insert into alsoKnownAs(personId, otherName) values (2, 'Rebecca Harman');
insert into alsoKnownAs(personId, otherName) values (2, 'Becca');
insert into alsoKnownAs(personId, otherName) values (2, 'Becca Gwenn');
insert into alsoKnownAs(personId, otherName) values (3, 'Mateo');
insert into alsoKnownAs(personId, otherName) values (3, 'Matt');
insert into alsoKnownAs(personId, otherName) values (4, 'Kirkold');

insert into connection(person1Id, person2Id) values(1, 2);
insert into connection(person1Id, person2Id) values(1, 3);
insert into connection(person1Id, person2Id) values(1, 4);
insert into connection(person1Id, person2Id) values(1, 5);

insert into connection(person1Id, person2Id) values(2, 4);

-- Math (sets)
create table theNullSet(x int primary key);
create table naturalNumbers(x int primary key);
create table primeNumbers(x int primary key);
create table powersOfTen(x int primary key);

insert into naturalNumbers(x) values (1);
insert into naturalNumbers(x) values (2);
insert into naturalNumbers(x) values (3);
insert into naturalNumbers(x) values (4);
insert into naturalNumbers(x) values (5);
insert into naturalNumbers(x) values (6);
insert into naturalNumbers(x) values (7);
insert into naturalNumbers(x) values (8);
insert into naturalNumbers(x) values (9);
insert into naturalNumbers(x) values (10);

insert into primeNumbers(x) values(1);
insert into primeNumbers(x) values(2);
insert into primeNumbers(x) values(3);
insert into primeNumbers(x) values(5);
insert into primeNumbers(x) values(7);

insert into powersOfTen(x) values(1);
insert into powersOfTen(x) values(10);
insert into powersOfTen(x) values(100);
insert into powersOfTen(x) values(1000);
insert into powersOfTen(x) values(10000)
