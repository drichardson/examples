<?php
/*
if ($db = sqlite_open('mysqlitedb', 0644, $sqliteerror)) { 
    #sqlite_query($db, 'CREATE TABLE foo (bar varchar(10))');
    sqlite_query($db, "INSERT INTO foo VALUES ('fnord')");
    $result = sqlite_query($db, 'select bar from foo');
    var_dump(sqlite_fetch_array($result)); 
} else {
    die($sqliteerror);
}
*/

$dbh = new PDO('sqlite:/apache2.2.6/htdocs/foo.db');
$dbh->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

try {
	$dbh->exec('CREATE TABLE foo (bar varchar(10))');
} catch(Exception $e) {
	echo "Failed to create table: " . $e->getMessage();
}

try {
	$dbh->exec("INSERT INTO foo (bar) values ('Test')");
} catch(Exception $e) {
	echo "Failed to insert: " . $e->getMessage();
}

try {
	$stmt = $dbh->prepare("INSERT INTO foo (bar) values (:barvalue)");
	$stmt->bindParam(':barvalue', $barvalue);
	
	$barvalue = "My 1st Val";
	$stmt->execute();
	
	$barvalue = "My 2nd Val";
	$stmt->execute();
	
	$barvalue = "%@#*%&%n"; # Evil input
	$stmt->execute();
	
} catch(Exception $e) {
	echo "Failed to insert prepared: " . $e->getMessage();
}
?>
