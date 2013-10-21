-- MySQL Administrator dump 1.4
--
-- ------------------------------------------------------
-- Server version	5.1.43-community


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;


--
-- Create schema ca
--

CREATE DATABASE IF NOT EXISTS ca;
USE ca;

--
-- Definition of table `decisions`
--

DROP TABLE IF EXISTS `decisions`;
CREATE TABLE `decisions` (
  `nino` varchar(9) NOT NULL,
  `description` varchar(255) DEFAULT NULL,
  `xml` text CHARACTER SET utf8,
  PRIMARY KEY (`nino`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='decision awards';



--
-- Definition of table `payments`
--

DROP TABLE IF EXISTS `payments`;
CREATE TABLE `payments` (
  `nino` varchar(9) NOT NULL,
  `description` varchar(255) DEFAULT NULL,
  `xml` text CHARACTER SET utf8 COMMENT 'XML document CLOB',
  PRIMARY KEY (`nino`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='payments';



--
-- Definition of table `validate_claims`
--

DROP TABLE IF EXISTS `validate_claims`;
CREATE TABLE `validate_claims` (
  `nino` varchar(9) NOT NULL,
  `description` varchar(255) DEFAULT NULL COMMENT 'Description of validate Claim',
  `xml` mediumtext CHARACTER SET utf8 COMMENT 'XML document CLOB',
  PRIMARY KEY (`nino`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COMMENT='claims data';





/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
