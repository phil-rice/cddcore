package org.cddcore.example.customerCategorisation

import scala.language.implicitConversions

import org.cddcore.engine._
import org.junit.runner.RunWith
import org.cddcore.engine.tests._
import org.cddcore.example.processCheque_DM_1.GBP
import org.corecdd.website.WebServer

case class Person(savings: GBP, ageInYears: Int) {
  lazy val hasEnoughSavings = savings >= 1000
  lazy val tooYoung = ageInYears < 16
}

@RunWith(classOf[CddJunitRunner])
object CategorisePerson {
  import GBP._

  val categorise = Engine[Person, String]().
    expected("person.valid").
    useCase("A person under 16 cannot use this system").
    scenario(Person(savings = 10000, ageInYears = 10)).expected("person.invalid.child").
    because((p: Person) => p.ageInYears < 16).

    scenario(Person(savings = 100, ageInYears = 15)).expected("person.invalid.child").

    useCase("Poor people").
    scenario(Person(savings = 50, ageInYears = 20)).expected("person.invalid.tooPoor").
    because((p: Person) => p.savings <1000).
    scenario(Person(savings = 999, ageInYears = 20)).expected("person.invalid.tooPoor").

    useCase("Rich people").
    scenario(Person(savings = 1050, ageInYears = 20)).
    because((p: Person) => p.hasEnoughSavings).
    build
    
    
  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

  def main(args: Array[String]) {
    println(categorise(Person( savings =100, ageInYears = 40)))
  }

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
//    param((s: String) => s.split(",") match { case Array(savings, age) => Person(Integer.parseInt(savings), Integer.parseInt(age)) }, "Savings,Age").
 

