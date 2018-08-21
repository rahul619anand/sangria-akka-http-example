import enumeratum.values.{IntCirceEnum, IntEnum, IntEnumEntry}
import sangria.macros.derive.{EnumTypeDescription, EnumTypeName, deriveEnumType}
import sangria.schema.EnumType

object Episode extends Enumeration {
  val NEWHOPE, EMPIRE, JEDI = Value
}

trait Character {
  def id: String
  def name: Option[String]
  def friends: List[String]
  def appearsIn: List[Episode.Value]
}

case class Human(
    id: String,
    name: Option[String],
    friends: List[String],
    appearsIn: List[Episode.Value],
    homePlanet: Option[String]
) extends Character

case class People(
    id: String,
    category: Category
)

object People {
  import io.circe._, io.circe.generic.semiauto._
  implicit val HumanDecoder: Decoder[People] = deriveDecoder
  implicit val HumanEncoder: Encoder[People] = deriveEncoder
}

case class Droid(
    id: String,
    name: Option[String],
    friends: List[String],
    appearsIn: List[Episode.Value],
    primaryFunction: Option[String]
) extends Character

class CharacterRepo {
  import CharacterRepo._

  def getHero(episode: Option[Episode.Value]) =
    episode flatMap (_ ⇒ getHuman("1000")) getOrElse droids.last

  def getHuman(id: String): Option[Human] = humans.find(c ⇒ c.id == id)

  def getDroid(id: String): Option[Droid] = droids.find(c ⇒ c.id == id)

  def getHumans(limit: Int, offset: Int): List[Human] =
    humans.drop(offset).take(limit)

  def setPeople(human: People): Boolean = true

  def getPeople(id: String): Option[People] = people.find(c ⇒ c.id == id)

  def getDroids(limit: Int, offset: Int): List[Droid] =
    droids.drop(offset).take(limit)
}

object CharacterRepo {
  val humans = List(
    Human(
      id = "1000",
      name = Some("Luke Skywalker"),
      friends = List("1002", "1003", "2000", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Tatooine")
    ),
    Human(
      id = "1001",
      name = Some("Darth Vader"),
      friends = List("1004"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Tatooine")
    ),
    Human(
      id = "1002",
      name = Some("Han Solo"),
      friends = List("1000", "1003", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = None
    ),
    Human(
      id = "1003",
      name = Some("Leia Organa"),
      friends = List("1000", "1002", "2000", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Alderaan")
    ),
    Human(
      id = "1004",
      name = Some("Wilhuff Tarkin"),
      friends = List("1001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = None
    )
  )

  val people = List(
    People(
      "1000",
      Category.Experiment
    ),
    People(
      "2000",
      Category.Control
    ),
    People(
      "3000",
      Category.Experiment
    )
  )

  val droids = List(
    Droid(
      id = "2000",
      name = Some("C-3PO"),
      friends = List("1000", "1002", "1003", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      primaryFunction = Some("Protocol")
    ),
    Droid(
      id = "2001",
      name = Some("R2-D2"),
      friends = List("1000", "1002", "1003"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      primaryFunction = Some("Astromech")
    )
  )
}

sealed abstract class Category(override val value: Int) extends IntEnumEntry

object Category extends IntEnum[Category] with IntCirceEnum[Category] {
  val values = findValues

  case object Experiment extends Category(1)
  case object Control extends Category(2)
  case object BugFix extends Category(3)
  case object KillSwitch extends Category(4)
  case object TechnicalTask extends Category(5)

  def apply(categoryId: Int): Category = Category.withValue(categoryId)

  implicit val CategoryEnumType = deriveEnumType[Category](
    EnumTypeName("CategoryEnumType"),
    EnumTypeDescription("experiment category")
  )
}
