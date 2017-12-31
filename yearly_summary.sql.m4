-- vim:set ft=sql:

.headers on
.mode column
.width 20 20 20
.echo on
SELECT
	CAST(SUM(a.distance) / 1000 AS INT) || " km" AS "Celkem najezd YEAR",
	CAST(SUM(a.movingTime) / 3600 AS INT) || "h" AS "Celkem cas pohybu YEAR"
FROM Activity a INNER JOIN Bike b ON b.stravaId = a.gearId
WHERE startTime BETWEEN 'YEAR-01-01' AND 'incr(YEAR)-01-01';

SELECT
	b.name AS "Kolo",
	CAST(SUM(a.distance) / 1000 AS INT) || " km" AS "Najezd",
	CAST(SUM(a.movingTime) / 3600 AS INT) || " h" AS "Cas pohybu"
FROM Activity a INNER JOIN Bike b ON b.stravaId = a.gearId
WHERE startTime BETWEEN 'YEAR-01-01' AND 'incr(YEAR)-01-01'
GROUP BY b.id ORDER BY SUM(a.distance) DESC;

SELECT
	ht.name AS "Hashtag",
	CAST(SUM(a.distance) / 1000 AS INT) || " km" AS "Najezd",
	CAST(SUM(a.movingTime) / 3600 AS INT) || " h" AS "Cas pohybu"
FROM
	Activity a INNER JOIN
	ActivityHashTag aht ON a.id = aht.activity INNER JOIN
	HashTag ht ON ht.id = aht.tag
WHERE startTime BETWEEN 'YEAR-01-01' AND 'incr(YEAR)-01-01'
GROUP BY ht.id
ORDER BY SUM(a.distance) DESC;
