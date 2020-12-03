\echo Find all the names someone is known by.
select name from
(select name, id from person
union
select otherName, personId from alsoKnownAs) as subquery1;

select p.name as Knower, pSubQuery.name as Knowee from connection c, person p,
(select name, id from person) as pSubQuery
where
c.person1Id = p.id and
c.person2Id = pSubQuery.id;


