package minionsgame.core
import scala.reflect.ClassTag
import scala.util.{Try,Success,Failure}

import RichImplicits._

/**
 * A whole bunch of standard types for the game.
 * Mostly immutable, except for a few obvious array-like types (SideArray, Plane).
 * See also Package.scala for some type aliases that Scala forces us to put in a separate file.
 */

/**
 * Side:
 * Side to move (i.e. minions is a 1 team vs 1 team game).
 */
sealed trait Side {
  val int: Int
  def opp: Side = this match { case S0 => S1  case S1 => S0 }

  override def toString: String = this match { case S0 => "S0"  case S1 => "S1" }
  def toColorName: String = this match { case S0 => "Blue"  case S1 => "Red" }
}
case object S0 extends Side { val int = 0 }
case object S1 extends Side { val int = 1 }
object Side {
  def ofString(s:String): Side = {
    s match {
      case "S0" => S0
      case "S1" => S1
      case "0" => S0
      case "1" => S1
      case _ => throw new Exception("Could not parse side: " + s)
    }
  }

  def foreach(f: Side => Unit): Unit = {
    f(S0)
    f(S1)
  }
  def exists(f: Side => Boolean): Boolean = f(S0) || f(S1)
  val sides = List(S0,S1)
}

/**
 * TargetEffect:
 * The effects of an attack or a spell on a target piece.
 */
sealed trait TargetEffect
case class Damage(damage: Int) extends TargetEffect
case object Unsummon extends TargetEffect
case object Kill extends TargetEffect
case class Enchant(modWithDuration: PieceModWithDuration) extends TargetEffect
case class TransformInto(name: PieceName) extends TargetEffect

/**
 * ActState:
 * Current status of piece during a turn, representing the basic structure move => attack => spawn
 * For purpose of ordering/comparison, greater states are those that come after earlier states.
 */
sealed trait ActState extends Ordered[ActState] {
  val order: Int
  def compare(that: ActState) : Int = {
    (this,that) match {
      case (Moving(x),Moving(y)) => x.compare(y)
      case (Attacking(x),Attacking(y)) => x.compare(y)
      case (_,_) => this.order.compare(that.order)
    }
  }
}
case class Moving(val steps: Int) extends ActState { val order = 0 }
case class Attacking(val strikes: Int) extends ActState { val order = 1 }
case object Spawning extends ActState { val order = 2 }
case object DoneActing extends ActState { val order = 3 }
object ActState {
  val start = Moving(0)
}

/**
 * PieceStats:
 * All the immutable stats of a piece. Also used to represent pieces in reinforcements (i.e. in hand).
 */
case class PieceStats(
  val name: PieceName,

  //displayName is for the GUI.
  //The separation this from PieceName allows us to make graphical and cosmetic changes to things like piece names without
  //invalidating recorded games (although of course there's no help for modifications to piece stats that affect gameplay!)
  val shortDisplayName: String,
  val displayName: String,
  //Indicates when a pieceStats has been modified by a spell or ablity
  val isBaseStats: Boolean,

  val attackEffect: Option[TargetEffect],
  val defense: Option[Int],
  val moveRange: Int,
  val attackRange: Int,
  val attackRangeVsFlying: Int,
  val numAttacks: Int,
  val productionCost: Int,
  val scienceCost: Int,

  val cost: Int,
  val rebate: Int,

  val isNecromancer: Boolean,
  val isFlying: Boolean,     //Can move over water, opposing enemies
  val isLumbering: Boolean,  //Cannot move and attack on the same turn
  val isPersistent: Boolean, //Cannot be unsummoned (sent back to reinforcements/hand)
  val isEldritch: Boolean,   //Can spawn next to any unit
  val isSoulbound: Boolean,  // Returns to reinforcements instead of dying
  val isWailing: Boolean,    //At the end of turn if it attacked, piece dies
  val canBlink: Boolean,     //Can move to reinforcements
  val canHurtNecromancer: Boolean, //Piece not allowed to attack necromancer

  val swarmMax: Int,   //Number of copies of piece with same name that can occupy a space
  val spawnRange: Option[Int], //Radius at which this unit can spawn reinforcements
  val extraSouls: Int,  //Souls generated by this piece per turn
  val extraMana: Int, //Mana generated by this piece per turn

  val deathSpawn: Option[PieceName], //Automatic spawn upon death
  val perTurnReinforcement: Option[PieceName], //Automatic gain to reinforcements every turn

  //Abilities that a piece can use by discarding a spell
  val abilities: List[PieceAbility],
  val notes: String,
  val notes2: String,

  val nimble: Boolean,
  val charge: Boolean,
  val retaliate: Boolean,
  val poisonous: Int,
  val robust: Int,
  val leadership: Boolean,
  val taunt: Boolean,
) {
  def toFragments() : (PieceStatsFragment0,PieceStatsFragment1) = {
    (
      PieceStatsFragment0(
        name = name,
        shortDisplayName = shortDisplayName,
        displayName = displayName,
        isBaseStats = isBaseStats,
        attackEffect = attackEffect,
        defense = defense,
        moveRange = moveRange,
        attackRange = attackRange,
        attackRangeVsFlying = attackRangeVsFlying,
        numAttacks = numAttacks,
        cost = cost,
        rebate = rebate,
        canBlink = canBlink,
        canHurtNecromancer = canHurtNecromancer,
        productionCost = productionCost,
        scienceCost = scienceCost,
        notes = notes,
        notes2 = notes2,
      ),
      PieceStatsFragment1(
        swarmMax = swarmMax,
        spawnRange = spawnRange,
        extraSouls = extraSouls,
        extraMana = extraMana,
        deathSpawn = deathSpawn,
        perTurnReinforcement = perTurnReinforcement,
        abilities = abilities,
        isNecromancer = isNecromancer,
        isFlying = isFlying,
        isLumbering = isLumbering,
        isPersistent = isPersistent,
        isEldritch = isEldritch,
        isSoulbound = isSoulbound,
        isWailing = isWailing,
        nimble = nimble,
        charge = charge,
        retaliate = retaliate,
        poisonous = poisonous,
        robust = robust,
        leadership = leadership,
        taunt = taunt,
      )
    )
  }
}

//Stupid hack because the JSON library we use doesn't support case classes with more than 22 fields
case class PieceStatsFragment0 (
  val name: PieceName,
  val shortDisplayName: String,
  val displayName: String,
  val isBaseStats: Boolean,
  val attackEffect: Option[TargetEffect],
  val defense: Option[Int],
  val moveRange: Int,
  val attackRange: Int,
  val attackRangeVsFlying: Int,
  val numAttacks: Int,
  val cost: Int,
  val rebate: Int,
  val canBlink: Boolean,
  val canHurtNecromancer: Boolean,
  val productionCost: Int,
  val scienceCost: Int,
  val notes: String,
  val notes2: String,
)
case class PieceStatsFragment1 (
  val swarmMax: Int,
  val spawnRange: Option[Int],
  val extraSouls: Int,
  val extraMana: Int,
  val deathSpawn: Option[PieceName],
  val perTurnReinforcement: Option[PieceName],
  val abilities: List[PieceAbility],
  val isNecromancer: Boolean,
  val isFlying: Boolean,
  val isLumbering: Boolean,
  val isPersistent: Boolean,
  val isEldritch: Boolean,
  val isSoulbound: Boolean,
  val isWailing: Boolean,
  val nimble: Boolean,
  val charge: Boolean,
  val retaliate: Boolean,  
  val poisonous: Int,
  val robust: Int,
  val leadership: Boolean,
  var taunt: Boolean,
)

object PieceStatsOfFragments {
  def ofFragments(f0: PieceStatsFragment0, f1: PieceStatsFragment1) : PieceStats = {
    PieceStats(
      name = f0.name,
      shortDisplayName = f0.shortDisplayName,
      displayName = f0.displayName,
      isBaseStats = f0.isBaseStats,
      attackEffect = f0.attackEffect,
      defense = f0.defense,
      moveRange = f0.moveRange,
      attackRange = f0.attackRange,
      attackRangeVsFlying = f0.attackRangeVsFlying,
      numAttacks = f0.numAttacks,
      cost = f0.cost,
      rebate = f0.rebate,
      canBlink = f0.canBlink,
      canHurtNecromancer = f0.canHurtNecromancer,
      productionCost = f0.productionCost,
      scienceCost = f0.scienceCost,
      notes = f0.notes,
      notes2 = f0.notes2,
      swarmMax = f1.swarmMax,
      spawnRange = f1.spawnRange,
      extraSouls = f1.extraSouls,
      extraMana = f1.extraMana,
      deathSpawn = f1.deathSpawn,
      perTurnReinforcement = f1.perTurnReinforcement,
      abilities = f1.abilities,
      isNecromancer = f1.isNecromancer,
      isFlying = f1.isFlying,
      isLumbering = f1.isLumbering,
      isPersistent = f1.isPersistent,
      isEldritch = f1.isEldritch,
      isSoulbound = f1.isSoulbound,
      isWailing = f1.isWailing,
      nimble = f1.nimble,
      charge = f1.charge,
      retaliate = f1.retaliate,
      poisonous = f1.poisonous,
      robust = f1.robust,
      leadership = f1.leadership,
      taunt = f1.taunt,
    )
  }
}


/**
 * PieceModWithDuration:
 * Essentially a tuple of a PieceMod and a number of turns that it lasts.
 */
case class PieceModWithDuration(
  val mod: PieceMod,
  val turnsLeft: Option[Int] //Counts down on EACH side's turn. None = Permanent
) {
  //Decay turnsLeft by one turn, returning None if the PieceMod decays away entirely.
  def decay: Option[PieceModWithDuration] = {
    turnsLeft match {
      case None => Some(this)
      case Some(turnsLeft) =>
        if(turnsLeft <= 1) None
        else Some(PieceModWithDuration(mod,Some(turnsLeft-1)))
    }
  }
}

/**
 * PieceAbility:
 * Ability that pieces can use.
 * Comparison is done using the name field only, which we rely on as a key to distinguish abilities
 * since functions are not comparable in Scala.
 */
trait PieceAbility {
  val name: AbilityName  //MUST be a UNIQUE key for different modifiers!
  val displayName: String
  val desc: List[String]
  val isSorcery: Boolean //Requires a discarded spell
  val spawnPhaseOnly: Boolean
  val tryIsUsableNow: Piece => Try[Unit]

  override def equals(o: Any): Boolean = o match {
    case that: PieceAbility => this.name == that.name
    case _ => false
  }
  override def hashCode: Int = name.hashCode
}

//Discard abilities that target the piece itself
case class SelfEnchantAbility(
  val name: AbilityName,
  val displayName: String,
  val desc: List[String],
  val isSorcery: Boolean,
  val spawnPhaseOnly: Boolean,
  val tryIsUsableNow: Piece => Try[Unit],
  val mod: PieceModWithDuration
) extends PieceAbility {
  override def equals(o: Any): Boolean = super.equals(o)
  override def hashCode: Int = super.hashCode
}
//Discard abilities that target another piece
case class TargetedAbility(
  val name: AbilityName,
  val displayName: String,
  val desc: List[String],
  val isSorcery: Boolean,
  val spawnPhaseOnly: Boolean,
  val tryIsUsableNow: Piece => Try[Unit],
  val tryCanTarget: (Piece, Piece) => Try[Unit], //(piece, target)
  val effect: TargetEffect
) extends PieceAbility {
  override def equals(o: Any): Boolean = super.equals(o)
  override def hashCode: Int = super.hashCode
}

/**
 * SpellType:
 * The type of a spell. Normal spells are just played as usual. Sorceries require discarding a spell.
 * Cantrips if discarded for a sorcery or ability can be played for their effect.
 * DoubleCantrips are cantrips that power two sorceries.
 */
sealed trait SpellType
case object NormalSpell extends SpellType
case object Sorcery extends SpellType
case object Cantrip extends SpellType
case object DoubleCantrip extends SpellType

/**
 * Spell:
 * A spell that necromancers can hold in hand and play.
 * Comparison is done using the key field only, which we rely on as a key to distinguish abilities
 * since functions are not comparable in Scala.
 */
sealed trait Spell {
  val name: SpellName
  val displayName: String
  val shortDisplayName: String
  val desc: List[String]
  val spellType: SpellType
  val spawnPhaseOnly: Boolean

  override def equals(o: Any): Boolean = o match {
    case that: Spell => this.name == that.name
    case _ => false
  }
  override def hashCode: Int = name.hashCode
}

/**
 * TargetedSpell:
 * Targets a single piece and applies an effect.
 */
case class TargetedSpell(
  val name: SpellName,
  val displayName: String,
  val shortDisplayName: String,
  val desc: List[String],
  val spellType: SpellType,
  val spawnPhaseOnly: Boolean,
  val tryCanTarget: (Side, Piece, BoardState) => Try[Unit], //(spell caster side, target, board)
  val effect: TargetEffect
) extends Spell {
  override def equals(o: Any): Boolean = super.equals(o)
  override def hashCode: Int = super.hashCode
}

/**
 * TileSpell:
 * Targets a single tile and applies an effect.
 */
case class TileSpell(
  val name: SpellName,
  val displayName: String,
  val shortDisplayName: String,
  val desc: List[String],
  val spellType: SpellType,
  val spawnPhaseOnly: Boolean,
  val tryCanTarget: (Side, Loc, BoardState) => Try[Unit], //(spell caster side, tile, board)
  val effect: ((BoardState,Loc) => Unit)
) extends Spell {
  override def equals(o: Any): Boolean = super.equals(o)
  override def hashCode: Int = super.hashCode
}

/**
 * PieceAndLocSpell:
 * Targets a single piece and a location.
 */
case class PieceAndLocSpell(
  val name: SpellName,
  val displayName: String,
  val shortDisplayName: String,
  val desc: List[String],
  val spellType: SpellType,
  val spawnPhaseOnly: Boolean,
  val tryCanTargetPiece: (Side, Piece) => Try[Unit],
  val tryCanTarget: (Side, Piece, Loc, BoardState) => Try[Unit],
  val effect: ((BoardState, Piece, Loc) => Unit)
) extends Spell {
  override def equals(o: Any): Boolean = super.equals(o)
  override def hashCode: Int = super.hashCode
}

case class TerrainAndTileSpell(
  val name: SpellName,
  val displayName: String,
  val shortDisplayName: String,
  val desc: List[String],
  val spellType: SpellType,
  val spawnPhaseOnly: Boolean,
  val tryCanTarget: (Side, Terrain, Loc, BoardState) => Try[Unit], //(spell caster side, tile, board)
  val effect: ((BoardState, Terrain, Loc) => Unit)
) extends Spell {
  override def equals(o: Any): Boolean = super.equals(o)
  override def hashCode: Int = super.hashCode
}

case class NoTargetSpell(
  val name: SpellName,
  val displayName: String,
  val shortDisplayName: String,
  val desc: List[String],
  val spellType: SpellType,
  val spawnPhaseOnly: Boolean,
  val effect: ((BoardState, Side) => Unit)
) extends Spell {
  override def equals(o: Any): Boolean = super.equals(o)
  override def hashCode: Int = super.hashCode
}


/**
 * Terrain:
 * The type of terrain of a single space on the board.
 */
sealed trait Terrain
case object Wall extends Terrain
case object Ground extends Terrain
case class Water(moveable: Boolean) extends Terrain
case object Graveyard extends Terrain
case object SorceryNode extends Terrain
case object Teleporter extends Terrain
case class Spawner(pieceName:PieceName) extends Terrain
case class Earthquake(moveable: Boolean) extends Terrain
case class Firestorm(moveable: Boolean) extends Terrain
case class Whirlwind(moveable: Boolean) extends Terrain
case object Mist extends Terrain

/**
 * Loc, Vec:
 * Basic integer points and vectors
 */
case class Vec(dx: Int, dy:Int) extends Ordered[Vec] {
  def +(v: Vec) = Vec(dx+v.dx, dy+v.dy)
  def -(v: Vec) = Vec(dx-v.dx, dy-v.dy)
  def +(l: Loc) = Loc(dx+l.x, dy+l.y)
  def compare(that: Vec): Int = Ordering[(Int,Int)].compare((dx,dy),(that.dx,that.dy))
}

case class Loc(x:Int, y:Int) {
  def +(v: Vec) = Loc(x+v.dx, y+v.dy)
  def -(l: Loc) = Vec(x-l.x, y-l.y)
  def compare(that: Loc): Int = Ordering[(Int,Int)].compare((x,y),(that.x,that.y))

  override def toString: String = {
    Loc.xCoordString(x) + Loc.yCoordString(y)
  }
}
object Loc {
  val zero: Loc = Loc(0,0)

  def xCoordString(x: Int): String = {
    if(x < 0)
      x.toString()
    else {
      var xx = x
      var s = ""
      var looping = true
      while(looping) {
        s = s + ('a' + (xx % 26)).toChar
        if(xx <= 25)
          looping = false
        else
          xx = xx / 26 - 1
      }
      s
    }
  }
  def yCoordString(y: Int): String = {
    y.toString()
  }
}

/**
 * SideArray:
 * Length-two array indexed by Side.
 */
object SideArray {
  def create[T:ClassTag](initial: T) = new SideArray[T](Array.fill[T](2)(initial))
  def createTwo[T:ClassTag](s0:T, s1:T) = new SideArray[T](Array(s0, s1))
  def createFn[T:ClassTag](fn: Side => T) = new SideArray[T](Array(fn(S0), fn(S1)))
  def ofArrayInplace[T:ClassTag](arr: Array[T]) = new SideArray(arr)
}
class SideArray[T:ClassTag] private (
  private val arr: Array[T]
) {
  def apply(s:Side): T = arr(s.int)
  def update(s:Side, elt: T): Unit = arr(s.int) = elt

  def copy(): SideArray[T] = new SideArray[T](arr.clone())

  def map[U:ClassTag](f: T => U): SideArray[U] = new SideArray[U](arr.map(f))
  def foreach(f: T => Unit): Unit = arr.foreach(f)
  def foldLeft[U](z: U)(f: (U,T) => U) = arr.foldLeft(z)(f)
  def find(f: T => Boolean): Option[T] = arr.find(f)
  def findMap[U](f: T => Option[U]): Option[U] = arr.findMap(f)
  def transform(f: T => T): Unit = { arr.transform(f); () }

  def toArrayInplace: Array[T] = arr
}

/**
 * Plane:
 * 2-dimensional array with configurable topology
 */
object Plane {
  def create[T:ClassTag](
    xSize: Int,
    ySize: Int,
    topology: PlaneTopology,
    initial: T
  ) : Plane[T] = {
    new Plane(xSize,ySize,topology,Array.fill[T](xSize*ySize)(initial))
  }
}
class Plane[T:ClassTag] (
  val xSize: Int,
  val ySize: Int,
  val topology: PlaneTopology,
  private val arr: Array[T]
) {
  def apply(x:Int, y:Int): T = arr(x + y * xSize)
  def apply(loc: Loc): T = arr(loc.x + loc.y * xSize)
  def update(x:Int, y:Int, elt: T): Unit = arr(x + y * xSize) = elt
  def update(loc: Loc, elt: T): Unit = arr(loc.x + loc.y * xSize) = elt

  def inBounds(x: Int, y: Int): Boolean = x >= 0 && x < xSize && y >= 0 && y < ySize
  def inBounds(loc: Loc): Boolean = inBounds(loc.x,loc.y)

  def copy(): Plane[T] = new Plane[T](xSize,ySize,topology,arr.clone())

  def map[U:ClassTag](f: T => U): Plane[U] = new Plane[U](xSize,ySize,topology,arr.map(f))
  def foreach(f: T => Unit): Unit = arr.foreach(f)
  def foldLeft[U](z: U)(f: (U,T) => U) = arr.foldLeft(z)(f)
  def find(f: T => Boolean): Option[T] = arr.find(f)
  def findMap[U](f: T => Option[U]): Option[U] = arr.findMap(f)
  def transform(f: T => T): Unit = { arr.transform(f); () }

  def findLoc(f: T => Boolean): Option[Loc] = {
    val idx = arr.indexWhere(f)
    if(idx == -1) None
    else Some(Loc(idx % xSize, idx/xSize))
  }

  def foreachLoc(f: Loc => Unit): Unit = {
    for(y <- 0 until ySize) {
      for (x <- 0 until xSize) {
        f(Loc(x,y))
      }
    }
  }

  def foreachi(f: (Loc, T) => Unit): Unit = {
    for(y <- 0 until ySize) {
      for (x <- 0 until xSize) {
        f(Loc(x,y), arr(x + y * xSize))
      }
    }
  }

  def filterLocs(f: Loc => Boolean): List[Loc] = {
    var ret: List[Loc] = Nil
    for(y <- 0 until ySize) {
      for (x <- 0 until xSize) {
        val loc = Loc(x,y)
        if(f(loc))
          ret = loc :: ret
      }
    }
    ret.reverse
  }

  def getArrayInplace: Array[T] = arr
}

/**
 * PlaneTopology:
 * Specifies the topology of Plane - i.e. the distance metric and adjacency relationships
 */
sealed trait PlaneTopology {
  val adjOffsets: List[Vec]
  val adjOffsetsRange2: List[Vec]
  val adjOffsetsRange3: List[Vec]  
  def adj(loc: Loc): Seq[Loc]
  def forEachAdj(loc: Loc)(f: Loc => Unit) : Unit
  def forEachAdjRange2(loc: Loc)(f: Loc => Unit) : Unit
  def forEachAdjRange3(loc: Loc)(f: Loc => Unit) : Unit
  def distance(loc0: Loc, loc1: Loc): Int

  def forEachReachable(loc: Loc)(f: (Loc,Int) => Boolean) : Unit = {
    var reached = Set[Loc]()
    var thisQueue = scala.collection.mutable.Queue[Loc]()
    thisQueue += loc

    var dist = 0
    while(!thisQueue.isEmpty) {
      val nextQueue = scala.collection.mutable.Queue[Loc]()
      thisQueue.foreach { loc =>
        if(!reached.contains(loc)) {
          reached += loc
          val doContinue = f(loc, dist)
          if(doContinue) {
            forEachAdj(loc) { adj => nextQueue.enqueue(adj) }
          }
        }
      }
      dist += 1
      thisQueue = nextQueue
    }
  }

  override def toString: String = this match {
    case SquareTopology => "SquareTopology"
    case ManhattanTopology => "ManhattanTopology"
    case HexTopology => "HexTopology"
  }
}
object PlaneTopology {
  def ofString(s:String): PlaneTopology = {
    s match {
      case "SquareTopology" => SquareTopology
      case "ManhattanTopology" => ManhattanTopology
      case "HexTopology" => HexTopology
      case _ => throw new Exception("Could not parse topology: " + s)
    }
  }
}

/**
 * RegularTopology:
 * Topologies in which every location is congruent to every other location, and with the same orientation
 * with respect to X and Y.
 */
sealed trait RegularTopology extends PlaneTopology {
  val adjOffsets: List[Vec]
  val adjOffsetsRange2: List[Vec]
  val adjOffsetsRange3: List[Vec]
  def distance(loc0: Loc, loc1: Loc): Int

  def adj(loc: Loc): Seq[Loc] = {
    adjOffsets.map { vec => loc + vec }
  }
  def forEachAdj(loc: Loc)(f: Loc => Unit) = {
    adjOffsets.foreach { vec => f(loc+vec) }
  }
  def forEachAdjRange2(loc: Loc)(f: Loc => Unit) = {
    adjOffsetsRange2.foreach { vec => f(loc+vec) }
  }
  def forEachAdjRange3(loc: Loc)(f: Loc => Unit) = {
    adjOffsetsRange3.foreach { vec => f(loc+vec) }
  }
}

case object SquareTopology extends RegularTopology {
  val adjOffsets: List[Vec] = List(Vec(-1,-1),Vec(0,-1),Vec(1,-1),Vec(-1,0),Vec(1,0),Vec(-1,1),Vec(0,1),Vec(1,1))
  val adjOffsetsRange2: List[Vec] = adjOffsets
  val adjOffsetsRange3: List[Vec] = adjOffsets
  def distance(loc0: Loc, loc1: Loc): Int = {
    Math.max(Math.abs(loc1.x-loc0.x),Math.abs(loc1.y-loc0.y))
  }
}
case object ManhattanTopology extends RegularTopology {
  val adjOffsets: List[Vec] = List(Vec(0,-1),Vec(-1,0),Vec(1,0),Vec(0,1))
  val adjOffsetsRange2: List[Vec] = adjOffsets
  val adjOffsetsRange3: List[Vec] = adjOffsets
  def distance(loc0: Loc, loc1: Loc): Int = {
    Math.abs(loc1.x-loc0.x) + Math.abs(loc1.y-loc0.y)
  }
}
case object HexTopology extends RegularTopology {
  val adjOffsets: List[Vec] = List(Vec(0,-1),Vec(1,-1),Vec(1,0),Vec(0,1),Vec(-1,1),Vec(-1,0))
  val adjOffsetsRange2: List[Vec] = adjOffsets ::: List(Vec(0,-2),Vec(1,-2),Vec(2,-2),Vec(2,-1),Vec(2,0),Vec(1,1),
                                                        Vec(0,2),Vec(-1,2),Vec(-2,2),Vec(-2,1),Vec(-2,0),Vec(-1,-1))
  val adjOffsetsRange3: List[Vec] = adjOffsetsRange2 ::: List(Vec(0,-3),Vec(1,-3),Vec(2,-3),Vec(3,-3),Vec(3,-2),Vec(3,-1),
                                                              Vec(3,0),Vec(2,1),Vec(1,2),Vec(0,3),Vec(-1,3),Vec(-2,3),
                                                              Vec(-3,3),Vec(-3,2),Vec(-3,1),Vec(-3,0),Vec(-2,-1),Vec(-1,-2))
  def distance(loc0: Loc, loc1: Loc): Int = {
    Math.max(Math.max(Math.abs(loc1.x-loc0.x),Math.abs(loc1.y-loc0.y)),Math.abs((loc1.y-loc0.y) + (loc1.x-loc0.x)))
  }
}
