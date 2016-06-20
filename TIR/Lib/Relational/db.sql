drop table farmer cascade;
drop table farm cascade;
drop table animal cascade;
drop table contaminated cascade;

CREATE TABLE Farmer(
	farmerid INTEGER,
	name VARCHAR(20),
	PRIMARY KEY (farmerid)
);
		
CREATE TABLE Farm (
	farmid INTEGER,
	farmname VARCHAR(20),
	county VARCHAR(15),
	owner INTEGER NOT NULL REFERENCES Farmer(farmerid),
	PRIMARY KEY (farmid)
);
					
CREATE TABLE Animal(
	animalid INTEGER,
	name VARCHAR(15),
	type VARCHAR(10),
	price NUMERIC(8,2),
	location INTEGER NOT NULL REFERENCES Farm(farmid),
	PRIMARY KEY (animalid)
);
									             
CREATE TABLE Contaminated(
     	farm INTEGER NOT NULL REFERENCES Farm(farmid),
	animal INTEGER NOT NULL REFERENCES Animal(animalid),
	type VARCHAR(4),
	PRIMARY KEY (farm,animal)
);

insert into Farmer values(1,'John MacKeen'); 
insert into Farmer values(2,'Anthony Sweeney'); 
insert into Farmer values(3,'Liam Lawless'); 
insert into Farmer values(4,'Brian Palmer'); 
insert into Farmer values(5,'Scott Tiger'); 
insert into Farmer values(6,'Harry Footnmouth'); 
insert into Farmer values(7,'Lisa MacMurphyMc'); 
insert into Farmer values(8,'Esmeralda Hound'); 

insert into Farm values(1,'Glenroe','Meathe',1);
insert into Farm values(2,'Glen Ross','Kilkenny',2);
insert into Farm values(3,'Highland','Kilkenny',3);
insert into Farm values(4,'Coast Garden','Sligo',4);
insert into Farm values(5,'Everlast','Dublin',4);
insert into Farm values(6,'God Fellow','Carlow',5);
insert into Farm values(7,'Free Range Chicken','Wicklow',8);
insert into Farm values(8,'Organic Choice','Galway',7);
insert into Farm values(9,'Meat Plant II','Galway',6);
insert into Farm values(10,'Meat Plant I','Galway',6);

insert into Animal values(1,'Cow1', 'cow', 48.5,1);
insert into Animal values(2,'Cow2', 'cow', 64.5,1);
insert into Animal values(3,'Cow3', 'cow', 61,1);
insert into Animal values(4,'Cow4', 'cow', 4.5,1);
insert into Animal values(5,'Cow6', 'cow', 11.5,1);
insert into Animal values(6,'Cow7', 'cow', 62,2);
insert into Animal values(7,'Cow8', 'cow', 73.5,2);
insert into Animal values(8,'Cow9', 'cow', 93,2);
insert into Animal values(9,'Cow0', 'cow', 35.5,2);
insert into Animal values(10,'Cow5', 'cow', 43,2);
insert into Animal values(11,'Linda', 'cow', 45,3);
insert into Animal values(12,'Bert', 'cow', 41.2,3);
insert into Animal values(13,'Bart', 'cow', 31.5,3);
insert into Animal values(14,'Rose', 'cow', 31,3);
insert into Animal values(15,'Homer', 'cow', 54.5,3);
insert into Animal values(16,'Cindy', 'cow', 46,3);
insert into Animal values(17,'Ronda', 'cow', 77.5,3);
insert into Animal values(18,'Sheep1', 'sheep', 65.5,4);
insert into Animal values(19,'Sheep2', 'sheep', 78,4);
insert into Animal values(20,'Cotton', 'sheep', 66,4);
insert into Animal values(21,'Merino', 'sheep', 66,5);
insert into Animal values(22,'Silky', 'sheep', 99.5,5);
insert into Animal values(23,'Tender', 'sheep', 66.5,5);
insert into Animal values(24,'Beefy', 'cow', 68,9);
insert into Animal values(25,'Angus', 'cow', 75.5,9);
insert into Animal values(26,'Angus II', 'cow', 54,9);
insert into Animal values(27,'Cow5', 'cow', 43,9);
insert into Animal values(28,'Linda', 'cow', 445,9);
insert into Animal values(29,'Bert II', 'cow', 41.2,9);
insert into Animal values(30,'Bart Jr', 'cow', 31.5,10);
insert into Animal values(31,'Rose Snr', 'cow', 31,10);
insert into Animal values(32,'Homer III', 'cow', 54.5,10);
insert into Animal values(33,'Cindy 2', 'cow', 46,10);
insert into Animal values(34,'Ronda', 'cow', 77.5,10);
insert into Animal values(35,'Sheep1', 'sheep', 65.5,10);
insert into Animal values(36,'Sheep2', 'sheep', 78,10);
insert into Animal values(37,'Cotton', 'sheep', 66,10);
insert into Animal values(38,'Merino II', 'sheep', 66,10);
insert into Animal values(39,'SilkyII', 'sheep', 99.5,10);
insert into Animal values(40,'Tender II', 'sheep', 66.5,6);
insert into Animal values(41,'Beefy II', 'cow', 68,6);
insert into Animal values(42,'Angus', 'cow', 75.5,6);
insert into Animal values(43,'Angus II', 'cow', 54,9);

insert into Animal values(44,'Cotton II', 'sheep', 36,9);
insert into Animal values(45,'Merino III', 'sheep', 56,7);
insert into Animal values(46,'SilkyIV', 'sheep', 14.5,7);
insert into Animal values(47,'Tender VI', 'sheep', 66.5,7);
insert into Animal values(48,'Beefy VII', 'cow', 68,7);
insert into Animal values(49,'Angus V', 'cow', 75.5,8);
insert into Animal values(50,'Angus O', 'cow', 56,8);
insert into Animal values(51,'Angus O++', 'cow', 58,8);
insert into Animal values(52,'Angus Meat', 'cow', 59,8);



insert into Contaminated values(1,1,'BSE');
insert into Contaminated values(1,2,'BSE');
insert into Contaminated values(1,3,'FM');
insert into Contaminated values(1,4,'FM');
insert into Contaminated values(1,5,'FM');

insert into Contaminated values(2,6,'FM');
insert into Contaminated values(2,7,'FM');
insert into Contaminated values(2,8,'FM');
insert into Contaminated values(2,9,'FM');
insert into Contaminated values(2,10,'FM');

insert into Contaminated values(3,11,'FM');
insert into Contaminated values(3,12,'FM');
insert into Contaminated values(3,13,'BSE');
insert into Contaminated values(3,14,'BSE');
insert into Contaminated values(3,15,'BSE');

insert into Contaminated values(3,16,'BSE');
insert into Contaminated values(3,17,'BSE');
insert into Contaminated values(4,18,'BSE');
insert into Contaminated values(4,19,'BSE');
insert into Contaminated values(4,20,'FM');

insert into Contaminated values(5,21,'FM');
insert into Contaminated values(5,22,'FM');
insert into Contaminated values(5,23,'FM');
insert into Contaminated values(9,24,'BSE');
insert into Contaminated values(9,25,'BSE');

insert into Contaminated values(9,26,'BSE');
insert into Contaminated values(9,27,'BSE');
insert into Contaminated values(9,28,'FM');
insert into Contaminated values(9,29,'FM');
insert into Contaminated values(10,30,'BSE');
insert into Contaminated values(10,31,'BSE');
insert into Contaminated values(10,32,'BSE');
insert into Contaminated values(10,33,'BSE');
insert into Contaminated values(10,34,'BSE');
insert into Contaminated values(10,35,'BSE');
insert into Contaminated values(10,36,'BSE');
insert into Contaminated values(10,37,'FM');
insert into Contaminated values(10,38,'FM');
insert into Contaminated values(6,39,'FM');
insert into Contaminated values(6,40,'FM');
insert into Contaminated values(6,41,'FM');
insert into Contaminated values(9,42,'FM');
insert into Contaminated values(9,43,'BSE');

