package minionsgame.core

import scala.util.{Try,Success,Failure,Random}
import scala.collection.immutable.Vector

import RichImplicits._

/**
  * The board state and various (often mutable) data structures and types relating directly to it,
  * for a single board.
  *
  * This is the base layer of the board implementation - interacting with the board is done via
  * PlayerAction, which represents a basic action performable in the game according to the rules.
  * Note that the model presented here in BoardState is that move/attack/spawn actions are arbitrarily
  * interleavable. Enforcing of a separation of spawn actions into a separate phase at the end of
  * the turn is done by action reordering in Board.scala, the next layer up. This allows the user
  * to input actions in arbitrary order, yet have the result be consistent with having a spawn phase.
  *
  * Also implemented are GeneralBoardActions - gaining spells and buying pieces.
  * These are recorded separately from PlayerActions. This is because they aren't subject to the same
  * undo/redo/reordering logic that player actions are subject to, since they involve interaction with the broader game.
  *
  * Given generalBoard action G and player action P, an invariant that should hold is:
  * - If P followed by G is legal, then G followed by P must also be legal and lead to the same board state.
  * - Also generalBoardActions commute with each other.
  *
  * See Board.scala for the next layer up in the board implementation stack.
  */

/**
  * PieceSpec:
  * Identifies a single piece on the board uniquely on a given turn, and in a way that
  * should be robust to reordering of actions within that turn.
  */
sealed trait PieceSpec
//Piece was present at start of turn and has this id
case class StartedTurnWithID(pieceId: Int) extends PieceSpec
//Piece spawned this turn - (piece's name, spawning location, nth spawn at that location)
case class SpawnedThisTurn(pieceName: PieceName, spawnLoc: Loc, nthAtLoc: Int) extends PieceSpec
with Ordered[SpawnedThisTurn] {
  def compare(that: SpawnedThisTurn): Int =
    Ordering[(PieceName,Int,Int,Int)].compare(
        (pieceName,spawnLoc.x,spawnLoc.y,nthAtLoc),
        (that.pieceName,that.spawnLoc.x,that.spawnLoc.y,that.nthAtLoc)
    )
}
object PieceSpec {
  val none: PieceSpec = StartedTurnWithID(-1)
}

package object Constants {
  val PRODUCTION_DECAY_RATE = 0.75;
  val SCIENCE_DECAY_RATE = 0.75
  val SUICIDE_TAX = 0.75
  val SCIENCE_FOR_NEW_CITY = 2.0
  val CITY_NAMES = List("Marseilles", "Pyongyang", "Karakorum", "Addis Ababa", "Merv", "Dublin", "Los Angeles", 
                        "Leningrad", "Munich", "Bern", "Aleppo", "Lothlorien", "Winterfell", "Qarth", "Osgiliath",
                        "Jimboomba", "The Hive", "Bunker 118", "Braavos", "Uskdarler", "Ingulath", "Cairo", "Montreal",
                        "Toronto", "Atlanta", "Nairobi", "Noah's Rainbow", "Tokyo", "Hong Kong", "Chongqing", "Lagos",
                        "Sunspear", "Moscow", "Jakarta", "Tehran", "Wuhan", "Riyadh", "Miami", "Khartoum", "Delhi",
                        "Baikonur", "Pune", "Dardogar", "Meereen", "Oldtown", "Lannisport", "Oslo", "Venice", "Barcelona",
                        "Tikal", "Palenque", "Riga", "Carthag", "Arrakeen", "Mos Eisley", "Coruscant", "Theed", "Nineveh",
                        "Nimrud", "Tel Aviv", "New Jericho", "Myr", "Ankara", "White Harbor", "Nassau", "Berkeley",
                        "Boston", "Tijuana", "Cape Town", "Kinshasa", "Calgary", "Tenochtitlan", "Cuzco", "Tar Valon",
                        "Caemlyn", "Kistel", "Temesvar", "Kaposvar", "Zirc", "Sarvahr", "Wakanda", "Ravnica", 
                        "New Capenna", "Zigatvar", "Caracas", "Santiago", "Havana", "Almaty", "Chichen Itza",
                      )
  val WONDER_INDEX_BY_NAME = Map("statue of zeus" -> 0, "fast food chains" -> 1, "dream twister" -> 2, "junkotron" -> 3,
                                 "cloning vats" -> 4)
  val POPULATION_BASE_COST = 3

  // At what population does the rate of food cost/city strength growth halve? 
  val CITY_POPULATION_KINK_1 = 7

  // At what population does the rate of food cost/city strength growth stop? 
  val CITY_POPULATION_KINK_2 = 27

  // Implied by the values above it
  val MAX_POPULATION_COST: Int = (CITY_POPULATION_KINK_2 - CITY_POPULATION_KINK_1) / 2 + CITY_POPULATION_KINK_1 + POPULATION_BASE_COST
}

/**
  * PlayerAction:
  * A single action taken by a player. Does not include "GeneralBoard" actions of gaining spells or buying pieces.
  * PlayerActions are immutable and their data should be independent of any of the mutable types
  * on the board. That is, it should be meaningful to ask whether an action would be legal
  * on a different board, or if the order of actions were rearranged, etc.
  *
  * Notes:
  * Movements: movements is a list because we need to support piece swaps, triangular rotations, etc.
  */
sealed trait PlayerAction {
  //Does this action directly involve pieceSpec?
  def involvesPiece(pieceSpec: PieceSpec): Boolean = {
    this match {
      case Movements(movements) =>
        movements.exists { case Movement(pSpec, _) => pieceSpec == pSpec }
      case Attack(aSpec,tSpec) => pieceSpec == aSpec || pieceSpec == tSpec
      case Spawn(spawnLoc,spawnName) =>
        pieceSpec match {
          case StartedTurnWithID(_) => false
          //Note that we don't check nthAtLoc - this means local undo will undo all units spawned on that hex with that name.
          case SpawnedThisTurn(pieceName,sLoc,_) => pieceName == spawnName && sLoc == spawnLoc
        }
      case ActivateTile(loc) =>
        pieceSpec match {
          case StartedTurnWithID(_) => false
          //Note that we don't check nthAtLoc - this means local undo will undo all units spawned on that hex.
          case SpawnedThisTurn(_,sLoc,_) =>
            sLoc == loc
        }
      case ActivateAbility(spec,_,targets) =>
        pieceSpec == spec ||
        pieceSpec == targets.target0 ||
        pieceSpec == targets.target1
      case Blink(spec,_) =>
        pieceSpec == spec
      case Teleport(spec,_,_) =>
        pieceSpec == spec
      case PlaySpell(_,targets) =>
        pieceSpec == targets.target0 ||
        pieceSpec == targets.target1
      case DiscardSpell(_) =>
        false
      case AddToQueue(_,_,_) =>
        false
      case ClearQueue(_,_,_) =>
        false
      case SetTarget(_,_) =>
        false
      case SetFocus(_,_) =>
        false
      case PieceSuicide(_) =>
        false
      case FoundCity(_,_) =>
        false
    }
  }

  //Does this action directly involve spellId?
  def involvesSpell(spellId: SpellId): Boolean = {
    this match {
      case Movements(_) => false
      case Attack(_,_) => false
      case Spawn(_,_) => false
      case ActivateTile(_) => false
      case ActivateAbility(_,_,_) => false
      case Teleport(_,_,_) => false
      case Blink(_,_) => false
      case PlaySpell(id,_) => spellId == id
      case DiscardSpell(id) => spellId == id
      case AddToQueue(_,_,_) => false
      case ClearQueue(_,_,_) => false
      case SetTarget(_,_) => false
      case SetFocus(_,_) => false
      case PieceSuicide(_) => false
      case FoundCity(_,_) => false
    }
  }

  //When undoing this action, also undo any subsequent actions involving the returned list of pieceSpecs
  def additionalUndoPieceSpecs(): List[PieceSpec] = {
    this match {
      case Movements(movements) => movements.map { case Movement(pSpec, _) => pSpec }
      case Attack(aSpec,tSpec) => List(aSpec,tSpec)
      case Spawn(_,_) => List()
      case ActivateTile(_) => List()
      case ActivateAbility(_,_,_) => List()
      case Blink(spec,_) => List(spec)
      case Teleport(spec,_,_) => List(spec)
      case PlaySpell(_,_) => List()
      case DiscardSpell(_) => List()
      case AddToQueue(_,_,_) => List() 
      case ClearQueue(_,_,_) => List()   
      case SetTarget(_,_) => List()
      case SetFocus(_,_) => List() 
      case PieceSuicide(_) => List()
      case FoundCity(_,_) => List()
    }
  }
}

case class Movements(movements: List[Movement]) extends PlayerAction
case class Attack(attackerSpec: PieceSpec, targetSpec: PieceSpec) extends PlayerAction
case class Spawn(spawnLoc: Loc, pieceName: PieceName) extends PlayerAction
case class ActivateTile(loc: Loc) extends PlayerAction
case class ActivateAbility(spec: PieceSpec, ability: PieceAbility, targets: SpellOrAbilityTargets) extends PlayerAction
case class Blink(pieceSpec: PieceSpec, src:Loc) extends PlayerAction
case class Teleport(pieceSpec: PieceSpec, src: Loc, dest: Loc) extends PlayerAction
case class PlaySpell(spellId: SpellId, targets: SpellOrAbilityTargets) extends PlayerAction
case class DiscardSpell(spellId: SpellId) extends PlayerAction
case class AddToQueue(pieceName: PieceName, selectedCityId: Int, isScience: Boolean) extends PlayerAction
case class ClearQueue(selectedCityId: Int, isScience: Boolean, clearEntireQueue: Boolean) extends PlayerAction
case class SetTarget(selectedCityId: Int, target: Loc) extends PlayerAction
case class SetFocus(selectedCityId: Int, focus: String) extends PlayerAction
case class PieceSuicide(selectedPieceId: Int) extends PlayerAction
case class FoundCity(side: Side, loc: Loc) extends PlayerAction


//Note: path should contain both the start and ending location
case class Movement(pieceSpec: PieceSpec, path: Vector[Loc])

case class SpellPlayedInfo(
  val spellId: SpellId,
  val side: Side,
  val targets: Option[SpellOrAbilityTargets]
)

//Data for the targets of a played spell or piece ability.
//Since different spells and abilities affect different things and have different numbers
//of targets, not all fields may be applicable.
case class SpellOrAbilityTargets(
  val target0: PieceSpec,
  val target1: PieceSpec,
  val loc0: Loc,
  val loc1: Loc,
  val terrain: Option[Terrain]
)
object SpellOrAbilityTargets {
  val none = new SpellOrAbilityTargets(PieceSpec.none,PieceSpec.none,Loc(-1,-1),Loc(-1,-1),None)
  def singlePiece(pieceSpec: PieceSpec) =
    new SpellOrAbilityTargets(pieceSpec,PieceSpec.none,Loc(-1,-1),Loc(-1,-1),None)
  def singleLoc(loc: Loc) =
    new SpellOrAbilityTargets(PieceSpec.none,PieceSpec.none,loc,Loc(-1,-1),None)
  def pieceAndLoc(pieceSpec: PieceSpec, loc: Loc) =
    new SpellOrAbilityTargets(pieceSpec,PieceSpec.none,loc,Loc(-1,-1),None)
  def terrainAndLoc(terrain: Terrain, loc: Loc) = new SpellOrAbilityTargets(PieceSpec.none, PieceSpec.none, loc, Loc(-1,-1), Some(terrain))
}

/** GeneralBoardAction:
  * Actions relating to this board that involve interaction with the broader game (a shared spell pool, a shared souls pool).
  * These are NOT part of the normal action stack.
  *
  * Requirement: spellId should be a unique identifier for a particular spell card. Users of BoardState should ensure that this is the case.
  */
sealed trait GeneralBoardAction {
  //Does this action involve buying the named piece?
  def involvesBuyPiece(pieceName: PieceName): Boolean = {
    this match {
      case BuyReinforcement(name,_) => pieceName == name
      case AddToScienceQueue(_,_) => false
      case GainSpell(_) => false
    }
  }

  //Does this action involve gaining the identified spell?
  def involvesGainSpell(spellId: SpellId): Boolean = {
    this match {
      case GainSpell(id) => spellId == id
      case AddToScienceQueue(_,_) => false
      case BuyReinforcement(_,_) => false
    }
  }

}

case class BuyReinforcement(pieceName: PieceName, free: Boolean) extends GeneralBoardAction
case class AddToScienceQueue(pieceName: PieceName, selectedCity: Int) extends GeneralBoardAction
case class GainSpell(spellId: SpellId) extends GeneralBoardAction

/** Tile:
 *  A single tile on the board
 *  Possibly enchanted due to spells. Later in list -> spells were played later.
 */
object Tile {
  def apply(terrain: Terrain): Tile = { 
    return new Tile(
      terrain, 
      terrain, 
      List(), 0,0,0,0,0,0,
      Map(),
      Map(),
      Map(),
    ) 
  }
}
case class Tile(
  val terrain: Terrain,
  val startingTerrain: Terrain,
  val modsWithDuration: List[PieceModWithDuration],
  var foodYield: Int,
  var productionYield: Int,
  var scienceYield: Int,
  var food: Double,  
  var production: Double,
  var science: Double,
  var hasNearbyEnemy: Map[Side, Boolean],
  var hasNearbyFriendly: Map[Side, Boolean],
  var isVisible: Map[Side, Boolean],
)

/**
 * Piece:
 * MUTABLE - be aware when copying, as we rely sharing of mutation to fields between different references to a piece
 * A single piece on the board.
 */
object Piece {
  //This function is used internally by the board implementation.
  //Users should never need to call this function. For setting up initial pieces, see functions like spawnPieceInitial.
  def createInternal(side: Side, pieceStats: PieceStats, id: Int, loc: Loc, nthAtLoc: Int): Piece = {
    new Piece(
      side = side,
      baseStats = pieceStats,
      id = id,
      loc = loc,
      target = loc,
      modsWithDuration = List(),
      damage = 0,
      actState = DoneActing,
      hasMoved = false,
      hasAttacked = nthAtLoc > 0,
      food = 0,
      production = 0,
      science = 0,
      carriedFood = 0,
      carriedProduction = 0,
      carriedScience = 0,
      productionQueue = List(),
      scienceQueue = List(),
      buildings = List(),
      population = 0,
      focus = "food",
      modifiers = (0, 0, "", false),
    )
  }
  def createInternal(side: Side, pieceStats: PieceStats, id: Int, loc: Loc, nthAtLoc: Int, target: Loc,
    food: Double, production: Double, science: Double): Piece = {
    new Piece(
      side = side,
      baseStats = pieceStats,
      id = id,
      loc = loc,
      target = target,
      modsWithDuration = List(),
      damage = 0,
      actState = DoneActing,
      hasMoved = false,
      hasAttacked = nthAtLoc > 0,
      food = food,
      production = production,
      science = science,
      carriedFood = 0,
      carriedProduction = 0,
      carriedScience = 0,
      productionQueue = List(),
      scienceQueue = List(),
      buildings = List(),
      population = 0,
      focus = "food",  
      modifiers = (0, 0, "", false),
    )
  }  
}
case class Piece (
  val side: Side,
  val baseStats: PieceStats,
  val id: Int,
  var loc: Loc, //BoardState is responsible for updating this as the piece moves
  //Modifiers from spells, etc, along with the number of turns they will last
  var target: Loc, //Where the piece is going for non-cities, where children will go for cities
  var modsWithDuration: List[PieceModWithDuration],
  //Damage dealt to this piece
  var damage: Int,
  //Indicates what this piece is allowed to do given what it's done
  var actState: ActState,
  //Indicates what this piece actually DID do this turn so far.
  var hasMoved: Boolean,
  var hasAttacked: Boolean,
  var food: Double,
  var production: Double,
  var science: Double,
  var carriedFood: Double,
  var carriedProduction: Double,
  var carriedScience: Double,

  //Properties of cities but not other units
  var productionQueue: List[PieceStats],
  var scienceQueue: List[PieceStats],
  var buildings: List[PieceStats],
  var population: Int,
  var focus: String,

  // Modifiers are poison, 
  var modifiers: (Int, Int, String, Boolean),
) {
  def copy() = {
    new Piece(
      side = side,
      baseStats = baseStats,
      id = id,
      loc = loc,
      target = target,
      modsWithDuration = modsWithDuration,
      damage = damage,
      hasMoved = hasMoved,
      hasAttacked = hasAttacked,
      actState = actState,
      food = food,
      production = production,
      science = science,
      carriedFood = carriedFood,
      carriedProduction = carriedProduction,
      carriedScience = carriedScience,
      productionQueue = productionQueue,
      scienceQueue = scienceQueue,
      buildings = buildings,
      population = population,
      focus = focus,
      modifiers = modifiers,
    )
  }

  //Taking into account all mods on this piece as well as the mods on the tile the piece is standing on
  def curStats(board: BoardState): PieceStats = {
    (modsWithDuration ++ board.tiles(loc).modsWithDuration).foldLeft(baseStats) {
      (pieceStats,mod) => mod.mod(pieceStats)
    }
  }

  //Get a spec that refers to this piece and can do so stably across undos or actions.
  def spec: PieceSpec = {
    StartedTurnWithID(id)
  }
}

/**
 * BoardState:
 * The full state of one board of the game.
 */
object BoardState {
  def create(terrain: Plane[Terrain], numPlayers: Int, seed: Int): BoardState = {
    val rand = new Random(seed)
    val board = new BoardState(
      tiles = terrain.map { terrain => {
          val randomFloat = rand.nextFloat;
          var foodYield: Int = 0;
          var productionYield: Int = 0;
          var scienceYield: Int = 0;
          val food: Double = 0.0;
          val production: Double = 0.0;
          val science: Double = 0.0;
          if(randomFloat >= 0.4 && randomFloat < 0.8){
            foodYield = foodYield + 1;
          }          
          if(randomFloat < 0.4){
            productionYield = productionYield + 1;
          }
          if(randomFloat >= 0.8) {
            scienceYield = scienceYield + 1;
          }    
          Tile(terrain = terrain, terrain, modsWithDuration = Nil, foodYield, productionYield, scienceYield,
            food,production,science, Map(), Map(), Map());
        }
      },  
      startLocs = SideArray.createTwo(Loc(1,1), Loc(2,2), Loc(3,3), Loc(4,4), Loc(5,5), Loc(6,6), Loc(7,7)),
      pieces = Plane.create(terrain.xSize,terrain.ySize,terrain.topology,Nil),
      pieceById = Map(),
      nextPieceId = 0,
      piecesSpawnedThisTurn = Map(),
      numPiecesSpawnedThisTurnAt = Map(),
      killedThisTurn = Nil,
      unsummonedThisTurn = Nil,
      allowedNecros = SideArray.create(List[PieceName]()),
      allowedFreeBuyPieces = SideArray.create(Set[PieceName]()),
      numFreeBuysAllowed = SideArray.create(0),
      turnNumber = 0,
      reinforcements = SideArray.create(Map()),
      spellsInHand = SideArray.create(Nil),
      spellsPlayed = Nil,
      mana = 0,
      hasUsedSpawnerTile = false,
      side = S0,
      hasWon = false,
      canMove = false,
      turnEndingImmediately = false,
      soulsThisRound = SideArray.create(0),
      totalSouls = SideArray.create(0),
      totalCosts = SideArray.create(0),
      cities = List(),
      turnsTillNextCityTemporaryModifier = Map(),
      turnsTillNextCityPermanentModifier = Map(),
      citiesFounded = Map(),
      salvagerBuildingsBuilt = Map(),
      numPlayers = numPlayers,
      randomizedCityNames = rand.shuffle(Constants.CITY_NAMES),
      wondersUnderConstruction = List(Map(), Map(), Map(), Map(),Map()),
      wondersBuilt = List(Map(), Map(), Map(), Map(),Map()),
      seed = seed,
    )
    board
  }

  //Some local functions that it's nice to have in scope
  object Imports {
    def requireSuccess(b: Try[Unit]) =
      b match { case Failure(exn) => throw exn case Success(()) => () }
    def failUnless(b: Boolean, message: String) =
      if(!b) throw new Exception(message)
    def failIf(b: Boolean, message: String) =
      if(b) throw new Exception(message)
    def fail(message: String) =
      throw new Exception(message)
    def failed[U](message: String): Try[U] =
      Failure(new Exception(message))
  }
}
import BoardState.Imports._

//Stupid hack because the JSON library we use doesn't support case classes with more than 22 fields
case class BoardStateFragment0 (
  val tiles: Plane[Tile],
  val startLocs: SideArray[Loc],
  val pieces: Plane[List[Piece]],
  var pieceById: Map[Int,Piece],
  var nextPieceId: Int,
  var piecesSpawnedThisTurn: Map[SpawnedThisTurn,Piece],
  var numPiecesSpawnedThisTurnAt: Map[Loc,Int],
  var killedThisTurn: List[(PieceSpec,PieceName,Side,Loc)],
  var unsummonedThisTurn: List[(PieceSpec,PieceName,Side,Loc)],
  var allowedNecros: SideArray[List[PieceName]],
  var allowedFreeBuyPieces: SideArray[Set[PieceName]],
  var numFreeBuysAllowed: SideArray[Int],
  var turnNumber: Int,
  var cities: List[Piece],
  var turnsTillNextCityTemporaryModifier: Map[Side, Int],
  var turnsTillNextCityPermanentModifier: Map[Side, Int],
  var citiesFounded: Map[Side, Int],
  var salvagerBuildingsBuilt: Map[Side, Int],
)
case class BoardStateFragment1 (
  val reinforcements: SideArray[Map[PieceName,Int]],
  val spellsInHand: SideArray[List[Int]],
  var spellsPlayed: List[SpellPlayedInfo],
  var mana: Int,
  var hasUsedSpawnerTile: Boolean,
  var side: Side,
  var hasWon: Boolean,
  var canMove: Boolean,
  var turnEndingImmediately: Boolean,
  val soulsThisRound: SideArray[Int],
  val totalSouls: SideArray[Int],
  val totalCosts: SideArray[Int],
  val numPlayers: Int,
  val randomizedCityNames: List[String],
  var wondersUnderConstruction: List[Map[Side, Boolean]],
  var wondersBuilt: List[Map[Side, Boolean]],
  var seed: Int,
)

object BoardStateOfFragments {
  def ofFragments(f0: BoardStateFragment0, f1: BoardStateFragment1) : BoardState = {
    BoardState(
      tiles = f0.tiles,
      startLocs = f0.startLocs,
      pieces = f0.pieces,
      pieceById = f0.pieceById,
      nextPieceId = f0.nextPieceId,
      piecesSpawnedThisTurn = f0.piecesSpawnedThisTurn,
      numPiecesSpawnedThisTurnAt = f0.numPiecesSpawnedThisTurnAt,
      killedThisTurn = f0.killedThisTurn,
      unsummonedThisTurn = f0.unsummonedThisTurn,
      allowedNecros = f0.allowedNecros,
      allowedFreeBuyPieces = f0.allowedFreeBuyPieces,
      numFreeBuysAllowed = f0.numFreeBuysAllowed,
      turnNumber = f0.turnNumber,
      cities = f0.cities,
      turnsTillNextCityTemporaryModifier = f0.turnsTillNextCityTemporaryModifier,
      turnsTillNextCityPermanentModifier = f0.turnsTillNextCityPermanentModifier,
      citiesFounded = f0.citiesFounded,
      salvagerBuildingsBuilt = f0.salvagerBuildingsBuilt,
      reinforcements = f1.reinforcements,
      spellsInHand = f1.spellsInHand,
      spellsPlayed = f1.spellsPlayed,
      mana = f1.mana,
      hasUsedSpawnerTile = f1.hasUsedSpawnerTile,
      side = f1.side,
      hasWon = f1.hasWon,
      canMove = f1.canMove,
      turnEndingImmediately = f1.turnEndingImmediately,
      soulsThisRound = f1.soulsThisRound,
      totalSouls = f1.totalSouls,
      totalCosts = f1.totalCosts,
      numPlayers = f1.numPlayers,
      randomizedCityNames = f1.randomizedCityNames,
      wondersUnderConstruction = f1.wondersUnderConstruction,
      wondersBuilt = f1.wondersBuilt,
      seed = f1.seed,
    )
  }
}

case class BoardState private (
  //For convenience, we leave these fields exposed rather than making them private and
  //carefully wrapping them in a bunch of getters and setters. But users of BoardState
  //should NOT modify any of these fields, only read them.

  //Tiles of the board
  val tiles: Plane[Tile],
  val startLocs: SideArray[Loc],
  //List of pieces in each space. Order is irrelevant
  val pieces: Plane[List[Piece]],

  //Map of all pieces currently on the board by pieceId.
  var pieceById: Map[Int,Piece],
  var nextPieceId: Int, //Counter for assigning per-board unique ids to pieces

  //Map of pieces spawned this turn
  var piecesSpawnedThisTurn: Map[SpawnedThisTurn,Piece],
  var numPiecesSpawnedThisTurnAt: Map[Loc,Int],
  //Pieces killed this turn
  var killedThisTurn: List[(PieceSpec,PieceName,Side,Loc)],
  var unsummonedThisTurn: List[(PieceSpec,PieceName,Side,Loc)],

  // This turn, a player can choose from these necromancers
  var allowedNecros: SideArray[List[PieceName]],
  //This turn, a player is allowed to freely buy any of these pieces
  var allowedFreeBuyPieces: SideArray[Set[PieceName]],
  var numFreeBuysAllowed: SideArray[Int],

  //Number of turns completed
  var turnNumber: Int,

  //Count of reinforcement pieces in hand by name
  val reinforcements: SideArray[Map[PieceName,Int]],

  //List of all spells in hand, indexed by spellID
  val spellsInHand: SideArray[List[Int]],
  // List of all spellIDs played this board
  var spellsPlayed: List[SpellPlayedInfo],

  //How many units of mana the side to move has (from cantrips and spell discards)
  var mana: Int,
  //Has the side to move used a spawner this turn?
  var hasUsedSpawnerTile: Boolean,

  //Current side to move
  var side: Side,
  //Has the current side won the board?
  var hasWon: Boolean,
  // Is the player allowed to move this turn?
  // False for a turn immediately after a graveyard victory
  var canMove: Boolean,
  //Set to true when a reset is happening that will be instantly followed by
  //a turn end, so that we don't update certain things as if this was "really" a turn.
  var turnEndingImmediately: Boolean,

  //Accumulated souls from spires and rebate for costs for units that died, this turn.
  //(Only clears at the beginning of a side's turn)
  val soulsThisRound: SideArray[Int],
  //Same, but never clears - summed over the whole board's lifetime.
  val totalSouls: SideArray[Int],
  //Total cost of units added to reinforcements of this board over the board's lifetime
  val totalCosts: SideArray[Int],

  var cities: List[Piece],
  var turnsTillNextCityTemporaryModifier: Map[Side, Int],
  var turnsTillNextCityPermanentModifier: Map[Side, Int],
  var citiesFounded: Map[Side, Int],
  var salvagerBuildingsBuilt: Map[Side, Int],
  val numPlayers: Int,
  val randomizedCityNames: List[String],
  var wondersUnderConstruction: List[Map[Side, Boolean]],
  var wondersBuilt: List[Map[Side, Boolean]],
  var seed: Int,
) {
  val xSize: Int = tiles.xSize
  val ySize: Int = tiles.ySize
  val topology: PlaneTopology = tiles.topology

  def toFragments() : (BoardStateFragment0,BoardStateFragment1) = {
    (
      BoardStateFragment0(
        tiles = tiles,
        startLocs = startLocs,
        pieces = pieces,
        pieceById = pieceById,
        nextPieceId = nextPieceId,
        piecesSpawnedThisTurn = piecesSpawnedThisTurn,
        numPiecesSpawnedThisTurnAt = numPiecesSpawnedThisTurnAt,
        killedThisTurn = killedThisTurn,
        unsummonedThisTurn = unsummonedThisTurn,
        allowedNecros = allowedNecros,
        allowedFreeBuyPieces = allowedFreeBuyPieces,
        numFreeBuysAllowed = numFreeBuysAllowed,
        turnNumber = turnNumber,
        cities = cities,
        turnsTillNextCityTemporaryModifier = turnsTillNextCityTemporaryModifier,
        turnsTillNextCityPermanentModifier = turnsTillNextCityPermanentModifier,
        citiesFounded = citiesFounded,
        salvagerBuildingsBuilt = salvagerBuildingsBuilt,
      ),
      BoardStateFragment1(
        reinforcements = reinforcements,
        spellsInHand = spellsInHand,
        spellsPlayed = spellsPlayed,
        mana = mana,
        hasUsedSpawnerTile = hasUsedSpawnerTile,
        side = side,
        hasWon = hasWon,
        canMove = canMove,
        turnEndingImmediately = turnEndingImmediately,
        soulsThisRound = soulsThisRound,
        totalSouls = totalSouls,
        totalCosts = totalCosts,
        numPlayers = numPlayers,
        randomizedCityNames = randomizedCityNames,
        wondersUnderConstruction = wondersUnderConstruction,
        wondersBuilt = wondersBuilt,
        seed = seed,
      )
    )
  }

  def copy(): BoardState = {
    val newBoard = new BoardState(
      tiles = tiles.copy(),
      startLocs = startLocs,
      pieces = pieces.copy(),
      pieceById = Map(), //Set below after construction
      nextPieceId = nextPieceId,
      piecesSpawnedThisTurn = Map(), //Set below after construction
      numPiecesSpawnedThisTurnAt = numPiecesSpawnedThisTurnAt,
      killedThisTurn = killedThisTurn,
      unsummonedThisTurn = unsummonedThisTurn,
      allowedNecros = allowedNecros.copy(),
      allowedFreeBuyPieces = allowedFreeBuyPieces.copy(),
      numFreeBuysAllowed = numFreeBuysAllowed.copy(),
      turnNumber = turnNumber,
      reinforcements = reinforcements.copy(),
      spellsInHand = spellsInHand.copy(),
      spellsPlayed = spellsPlayed,
      mana = mana,
      hasUsedSpawnerTile = hasUsedSpawnerTile,
      side = side,
      hasWon = hasWon,
      canMove = canMove,
      turnEndingImmediately = turnEndingImmediately,
      soulsThisRound = soulsThisRound.copy(),
      totalSouls = totalSouls.copy(),
      totalCosts = totalCosts.copy(),
      cities = cities,
      turnsTillNextCityTemporaryModifier = turnsTillNextCityTemporaryModifier,
      turnsTillNextCityPermanentModifier = turnsTillNextCityPermanentModifier,
      citiesFounded = citiesFounded,
      salvagerBuildingsBuilt = salvagerBuildingsBuilt,
      numPlayers = numPlayers,
      randomizedCityNames = randomizedCityNames,
      wondersUnderConstruction = wondersUnderConstruction,
      wondersBuilt = wondersBuilt,
      seed = seed,
    )
    val newPieceById = pieceById.transform({ (_k, piece) => piece.copy() })
    val newPiecesSpawnedThisTurn = piecesSpawnedThisTurn.transform { (_k, piece) => newPieceById(piece.id) }
    newBoard.pieceById = newPieceById
    newBoard.piecesSpawnedThisTurn = newPiecesSpawnedThisTurn
    newBoard.pieces.transform { pieceList => pieceList.filter(piece => newPieceById.contains(piece.id)).map { 
      piece => newPieceById(piece.id) 
    } }
    newBoard
  }

  //Check whether an action would be legal
  def tryLegality(action: PlayerAction, externalInfo: ExternalInfo): Try[Unit] = {
    tryLegalitySingle(action,externalInfo)
  }

  def numCitiesFounded(): Int = {
    var total = 0
    Side.foreach { side =>
      total += citiesFounded.get(side).getOrElse(0)
    }
    return total
  }

  //Check whether a sequence of actions would be legal
  def tryLegality(actions: Seq[PlayerAction], externalInfo: ExternalInfo): Try[Unit] = {
    val list = actions.toList
    list match {
      case Nil => Success(())
      case action :: Nil => tryLegalitySingle(action,externalInfo)
      case _ =>
        val board = this.copy()
        def loop(list: List[PlayerAction]): Try[Unit] = {
          list match {
            case Nil => Success(())
            case action :: Nil => board.tryLegalitySingle(action,externalInfo)
            case action :: rest =>
              board.doActionSingle(action,externalInfo) match {
                case Success(()) => loop(rest)
                case Failure(err) => Failure(err)
              }
          }
        }
        loop(list)
    }
  }

  def wonderUnderConstructionBySide(wonderName: String, side: Side): Boolean = {
    return wondersUnderConstruction(Constants.WONDER_INDEX_BY_NAME(wonderName)).get(side).getOrElse(false)
  }

  def wonderBuiltBySide(wonderName: String, side: Side): Boolean = {
    return wondersBuilt(Constants.WONDER_INDEX_BY_NAME(wonderName)).get(side).getOrElse(false)
  }

  def wonderBuilt(wonderName: String): Boolean = {
    Side.foreach { side =>
      if (wonderBuiltBySide(wonderName, side)) {
        return true
      }
    }
    return false
  }

  def markWonderUnderConstructionBySide(wonderName: String, side: Side, underConstruction: Boolean = true): Unit = {
    val mapIndex = Constants.WONDER_INDEX_BY_NAME(wonderName)
    var map = wondersUnderConstruction(mapIndex)
    map = map + (side -> underConstruction)
    val beginning = wondersUnderConstruction.slice(0, mapIndex)
    val end = wondersUnderConstruction.slice(mapIndex + 1, wondersUnderConstruction.length)
    wondersUnderConstruction = beginning ::: List(map)
    wondersUnderConstruction = wondersUnderConstruction ::: end
  }

  def markWonderBuiltBySide(wonderName: String, side: Side, built: Boolean = true): Unit = {
    val mapIndex = Constants.WONDER_INDEX_BY_NAME(wonderName)
    var map = wondersBuilt(mapIndex)
    map = map + (side -> built)
    val beginning = wondersBuilt.slice(0, mapIndex)
    val end = wondersBuilt.slice(mapIndex + 1, wondersBuilt.length)
    wondersBuilt = beginning ::: List(map)
    wondersBuilt = wondersBuilt ::: end
  }

  def clearWonderFromQueues(wonderName: String): Unit = {
    cities.foreach { city => 
      if (city.scienceQueue.length > 0) {
        val scienceQueueNotFirst = city.scienceQueue.slice(1, city.scienceQueue.length)
        city.scienceQueue = List(city.scienceQueue.head) ::: scienceQueueNotFirst.filterNot { p => p.name == wonderName }
      }
    }
  }

  def isWonder(name: String): Boolean = {
    return Constants.WONDER_INDEX_BY_NAME.contains(name)
  }

  //Perform an action
  def doAction(action: PlayerAction, externalInfo: ExternalInfo): Try[Unit] = {
    doActionSingle(action,externalInfo)
  }

  //Perform a sequence of actions, except if any part of the sequence is illegal, perform none of them.
  def doActions(actions: Seq[PlayerAction], externalInfo: ExternalInfo): Try[Unit] = {
    val list = actions.toList
    list match {
      case Nil => Success(())
      case action :: Nil => doActionSingle(action,externalInfo)
      case _ =>
        tryLegality(actions, externalInfo) match {
          case Failure(err) => Failure(err)
          case Success(()) =>
            list.foreach { action => doActionUnsafeAssumeLegal(action,externalInfo) }
            Success(())
        }
    }
  }

  //Perform an action, assuming that it's legal without checking. Could lead to
  //an exception or an invalid board state if it actually isn't legal
  def doActionUnsafeAssumeLegal(action: PlayerAction, externalInfo: ExternalInfo): Unit = {
    doActionSingleAssumeLegal(action,externalInfo)
  }

  def resetMana(): Unit = {
    var newMana = 0
    tiles.foreachi { case (loc,tile) =>
      val occupied = pieceById.values.exists { piece => piece.side == side && piece.loc == loc }
      if(tile.terrain == SorceryNode && occupied)
        newMana += 1
    }
    pieceById.values.foreach { piece =>
      if(piece.side == side) {
        val stats = piece.curStats(this)
        newMana += stats.extraMana
      }
    }
    mana = newMana
  }

  def endOfTurnSouls(side : Side) : Int = {
    var newSouls = countGraveyards(side)
    pieceById.values.foreach { piece =>
      if(piece.side == side) {
        val stats = piece.curStats(this)
        newSouls += stats.extraSouls
      }
    }
    newSouls
  }

  def soulsOnBoard(side: Side): Int = {
    pieceById.values.foldLeft(0) { case (ans, piece) =>
      if(piece.side == side) {
        val stats = piece.curStats(this)
        ans + stats.cost
      } else {
        ans
      }
    }
  }

  //What spells would need to be discarded to meet mana requirements?
  //This is NOT handled within the endTurn function, but rather by the server so that we can also get proper spell revealing.
  def spellsToAutoDiscardBeforeEndTurn(externalInfo: ExternalInfo): List[SpellId] = {
    var manaTmp = mana
    var discardCount = 0
    while(manaTmp < 0 && discardCount < spellsInHand(side).length) {
      val spellId = spellsInHand(side)(discardCount)
      val spell = Spells.spellMap(externalInfo.spellsRevealed(spellId))
      manaTmp += manaOfDiscard(spell.spellType)
      discardCount += 1
    }
    spellsInHand(side).take(discardCount)
  }

  private def gainStartOfTurnReinforcements() : Unit = {
    pieceById.values.foreach { piece =>
      if(piece.side == side) {
        val stats = piece.curStats(this)
        stats.perTurnReinforcement.foreach { pieceName =>
          addReinforcementInternal(side,pieceName)
        }
      }
    }
  }

  private def handleStartOfTurn(): Unit = {
    //Handle mana for the new turn
    resetMana()

    //Clear souls for the side to move, and other board state
    soulsThisRound(side) = 0
    piecesSpawnedThisTurn = Map()
    numPiecesSpawnedThisTurnAt = Map()
    killedThisTurn = Nil
    unsummonedThisTurn = Nil
    hasUsedSpawnerTile = false

    //Gain any free pieces we're supposed to, but only on turns we can actually play
    if(!turnEndingImmediately && canMove)
      gainStartOfTurnReinforcements()

    //Check for win conditions - start of turn at least 8 graveyards
    if(countGraveyards(side) >= 8) {
      hasWon = true
    }
  }

  //End the current turn and begin the next turn
  def endTurn(externalInfo: ExternalInfo): Unit = {
    //Wailing units that attacked and have not been finished yet die
    killAttackingWailingUnits(externalInfo)

    val newSouls = endOfTurnSouls(side)
    // Mist
    tiles.foreachLoc { loc =>
      if(tiles(loc).terrain == Mist) {
        pieces(loc).foreach { piece =>
          if(!piece.curStats(this).isPersistent) {
            unsummonPiece(piece)
          }
        }
      }
      resetHasNearbyFriendlyAndEnemy(loc)
    }
    //Heal damage, reset piece state, decay modifiers
    pieceById.keySet.toList.sorted.foreach { pieceId =>
      if (pieceById.contains(pieceId)) {
        val piece = pieceById(pieceId)
        refreshPieceForStartOfTurnWithAttackMove(piece, externalInfo)
        piece.modsWithDuration = piece.modsWithDuration.flatMap(_.decay)
      }
    }

    // Set targets from leadership units; must happen after all the moving is finished
    pieceById.keySet.toList.sorted.foreach { pieceId =>
      if (pieceById.contains(pieceId)) {
        val piece = pieceById(pieceId)
        if (piece.baseStats.leadership) {
          setTargetsOfFollowers(piece)
        }
        piece.side match {
          case SB =>
          case (S0 | S1 | S2 | S3 | S4 | S5) =>
            setNearbyTileCityFoundingData(piece)
          }
      }
    }

    //Decay tile modifiers
    tiles.transform { tile =>
      if(tile.modsWithDuration.isEmpty)
        tile
      else
        tile.copy(modsWithDuration = tile.modsWithDuration.flatMap(_.decay))
    }

    //Automatically apply free buys of pieces - take the most expensive piece.
    //Tiebreak by index in all pieces
    //We don't do this though if this is a turn end instantly after a reset, since
    //there is no time for the player to have non-automatedly chosen what piece they want.
    if(!turnEndingImmediately) {
      while(numFreeBuysAllowed(side) > 0) {
        var bestPieceName: Option[String] = None
        var bestPieceCost: Int = -1
        var bestPieceIdx: Int = -1
        allowedFreeBuyPieces(side).foreach { pieceName =>
          val cost = externalInfo.pieceMap(pieceName).cost
          val idx = Units.allPiecesIdx(pieceName)
          if(cost > bestPieceCost || (cost == bestPieceCost && idx > bestPieceIdx)) {
            bestPieceName = Some(pieceName)
            bestPieceCost = cost
            bestPieceIdx = idx
          }
        }
        bestPieceName.foreach { pieceName =>
          addReinforcementInternal(side,pieceName)
        }
        numFreeBuysAllowed(side) -= 1
      }
      allowedFreeBuyPieces(side) = Set()
      numFreeBuysAllowed(side) = 0

      // Can no longer rotate necromancers
      if(canMove) {
        allowedNecros(side) = List()
        pieceById.values.foreach { piece =>
          if(piece.side == side) {
            piece.modsWithDuration = piece.modsWithDuration.filter(x => x.mod != PieceMods.NecroPick)
          }
        }
      }
    }

    soulsThisRound(side) += newSouls
    totalSouls(side) += newSouls

    //Flip turn
    side = side.opp
    turnNumber += 1
    canMove = true
    turnEndingImmediately = false

    handleStartOfTurn()
  }

  //Reset the board to the starting position
  def resetBoard(necroNames: SideArray[List[PieceName]], canMoveFirstTurn: Boolean, turnEndingImmediatelyAfterReset: Boolean, new_reinforcements: SideArray[Map[PieceName, Int]], externalInfo: ExternalInfo): Unit = {
    //Keep a piece around though as a free buy
    Side.foreach { side =>
      numFreeBuysAllowed(side) = 1
      //We do NOT do clear allowedFreeBuyPieces(side) so that if somehow the board is resetting while
      //the player had free buys left over previously, they still get to choose those now.
      pieceById.values.foreach { piece =>
        if(piece.side == side && !piece.baseStats.isNecromancer)
          allowedFreeBuyPieces(side) = allowedFreeBuyPieces(side) + piece.baseStats.name
      }
      reinforcements(side).foreach { case (name,count) =>
        if(count > 0 && !externalInfo.pieceMap(name).isNecromancer) {
          allowedFreeBuyPieces(side) = allowedFreeBuyPieces(side) + name
        }
      }
      allowedNecros(side) = necroNames(side)
    }

    // Reset terrain and remove tile modifiers
    tiles.transform { tile => {
        val rand = new Random(seed);
        seed = rand.nextInt(100000)
        val randomFloat = rand.nextFloat;
        var foodYield: Int = 0;
        var productionYield: Int = 0;
        var scienceYield: Int = 0;
        val food: Double = 0.0;
        val production: Double = 0.0;
        val science: Double = 0.0;
        if(randomFloat >= 0.4 && randomFloat < 0.8){
          foodYield = foodYield + 1;
        }          
        if(randomFloat < 0.4){
          productionYield = productionYield + 1;
        }
        if(randomFloat >= 0.8) {
          scienceYield = scienceYield + 1;
        }    
        Tile(tile.startingTerrain, tile.startingTerrain, List(), foodYield,productionYield,scienceYield,food,production,science, Map(), Map(), Map())
      } 
    }

    //Remove all pieces and reinforcements
    pieces.transform { _ => Nil }
    pieceById = Map()
    piecesSpawnedThisTurn = Map()
    numPiecesSpawnedThisTurnAt = Map()
    spellsPlayed = Nil
    Side.foreach { side =>
      reinforcements(side) = new_reinforcements(side)
    }
    totalSouls.transform { _ => 0 }
    totalCosts.transform { _ => 0 }

    //Unset win flag
    hasWon = false
    canMove = canMoveFirstTurn
    turnEndingImmediately = turnEndingImmediatelyAfterReset

    val numAnomalies = 4 * numPlayers

    var playerStarts: List[(Side, Loc)] = List()
    var barbarianStarts: List[(PieceStats, Loc)] = List()

    if (numPlayers == 2) {
      playerStarts = List(
        (S0, Loc(1,6)),
        (S1, Loc(11,6)),
      )      
      barbarianStarts = List(
        (externalInfo.pieceMap("lair"), Loc(6,6)),
        (externalInfo.pieceMap("camp"), Loc(2,9)),
        (externalInfo.pieceMap("camp"), Loc(5,10)),
        (externalInfo.pieceMap("camp"), Loc(7,2)),
        (externalInfo.pieceMap("camp"), Loc(10,3)),
      )      
    }

    if (numPlayers == 3) {
      playerStarts = List(
        (S0, Loc(1,7)),
        (S1, Loc(13,1)),
        (S2, Loc(7,13)),
      )
      barbarianStarts = List(
        (externalInfo.pieceMap("lair"), Loc(7,7)),
        (externalInfo.pieceMap("camp"), Loc(7,2)),
        (externalInfo.pieceMap("camp"), Loc(4,7)),
        (externalInfo.pieceMap("camp"), Loc(2,12)),
        (externalInfo.pieceMap("camp"), Loc(7,10)),
        (externalInfo.pieceMap("camp"), Loc(12,7)),
        (externalInfo.pieceMap("camp"), Loc(10,4)),
      )
    }

    if (numPlayers == 4) {
      playerStarts = List(
        (S0, Loc(1,11)),
        (S1, Loc(7,17)),
        (S2, Loc(11,1)),
        (S3, Loc(17,7)),
      )

      barbarianStarts = List(
        (externalInfo.pieceMap("lair"), Loc(9,9)),
        (externalInfo.pieceMap("camp"), Loc(6,12)),
        (externalInfo.pieceMap("camp"), Loc(12,6)),
        (externalInfo.pieceMap("camp"), Loc(7,7)),
        (externalInfo.pieceMap("camp"), Loc(11,11)),
        (externalInfo.pieceMap("camp"), Loc(2,16)),
        (externalInfo.pieceMap("camp"), Loc(16,2)),
        (externalInfo.pieceMap("camp"), Loc(5,5)),
        (externalInfo.pieceMap("camp"), Loc(13,13)),
        (externalInfo.pieceMap("camp"), Loc(5,10)),
        (externalInfo.pieceMap("camp"), Loc(10,5)),
        (externalInfo.pieceMap("camp"), Loc(8,13)),
        (externalInfo.pieceMap("camp"), Loc(13,8)),
      )
    }

    if (numPlayers == 5) {
      playerStarts = List(
        (S0, Loc(9,2)),
        (S1, Loc(19,2)),
        (S2, Loc(16,13)),
        (S3, Loc(6,18)),
        (S4, Loc(1,13)),
      )

      barbarianStarts = List(
        (externalInfo.pieceMap("lair"), Loc(10,10)),
        (externalInfo.pieceMap("camp"), Loc(14,2)),
        (externalInfo.pieceMap("camp"), Loc(12,6)),
        (externalInfo.pieceMap("camp"), Loc(10,5)),
        (externalInfo.pieceMap("camp"), Loc(15,5)),
        (externalInfo.pieceMap("camp"), Loc(14,8)),
        (externalInfo.pieceMap("camp"), Loc(18,7)),
        (externalInfo.pieceMap("camp"), Loc(14,11)),
        (externalInfo.pieceMap("camp"), Loc(11,13)),
        (externalInfo.pieceMap("camp"), Loc(11,17)),
        (externalInfo.pieceMap("camp"), Loc(8,14)),
        (externalInfo.pieceMap("camp"), Loc(2,17)),
        (externalInfo.pieceMap("camp"), Loc(6,13)),
        (externalInfo.pieceMap("camp"), Loc(5,11)),
        (externalInfo.pieceMap("camp"), Loc(8,8)),
        (externalInfo.pieceMap("camp"), Loc(5,7)),
      )
    }    

    if (numPlayers == 6) {
      playerStarts = List(
        (S0, Loc(11,1)),
        (S1, Loc(21,1)),
        (S2, Loc(21,11)),
        (S3, Loc(11,21)),
        (S4, Loc(1,21)),
        (S5, Loc(1,11)),        
      )

      barbarianStarts = List(
        (externalInfo.pieceMap("lair"), Loc(11,11)),
        (externalInfo.pieceMap("camp"), Loc(16,1)),
        (externalInfo.pieceMap("camp"), Loc(13,7)),
        (externalInfo.pieceMap("camp"), Loc(11,5)),
        (externalInfo.pieceMap("camp"), Loc(15,9)),
        (externalInfo.pieceMap("camp"), Loc(21,6)),
        (externalInfo.pieceMap("camp"), Loc(17,5)),
        (externalInfo.pieceMap("camp"), Loc(17,11)),
        (externalInfo.pieceMap("camp"), Loc(13,13)),
        (externalInfo.pieceMap("camp"), Loc(16,16)),
        (externalInfo.pieceMap("camp"), Loc(11,17)),
        (externalInfo.pieceMap("camp"), Loc(9,15)),
        (externalInfo.pieceMap("camp"), Loc(6,21)),
        (externalInfo.pieceMap("camp"), Loc(5,17)),
        (externalInfo.pieceMap("camp"), Loc(7,13)),
        (externalInfo.pieceMap("camp"), Loc(1,16)),
        (externalInfo.pieceMap("camp"), Loc(5,11)),
        (externalInfo.pieceMap("camp"), Loc(9,9)),
        (externalInfo.pieceMap("camp"), Loc(6,6)),
      )
    }    


    val anomalies = 1 to numAnomalies

    anomalies.foreach { _ =>
      makeAnomalyWithAdjacentBarrens(getRandomLoc())
    }

    //Set up initial pieces
    playerStarts.foreach { tup =>
      val (side, startLoc) = tup;
      val cityStats = externalInfo.pieceMap("city")
      // 4P start
      spawnPieceInitial(side,cityStats,startLoc,startLoc,0,0,Constants.SCIENCE_FOR_NEW_CITY)
      // Guarantee that starting city yields from surrounding hexes are 2/2/2
      var counter = 0
      topology.forEachAdj(startLoc) { loc =>
        if (locIsValid(loc)) {
          val tile = tiles(loc)
          if (counter % 3 == 0) {
            tile.foodYield = 1
            tile.productionYield = 0
            tile.scienceYield = 0
          }
          if (counter % 3 == 1) {
            tile.foodYield = 0
            tile.productionYield = 1
            tile.scienceYield = 0
          }
          if (counter % 3 == 2) {
            tile.foodYield = 0
            tile.productionYield = 0
            tile.scienceYield = 1
          }
          counter += 1
        }
      }
      val tile = tiles(startLoc)
      tile.foodYield = 0
      tile.productionYield = 0
      tile.scienceYield = 0
      val rand = new Random(seed)
      seed = rand.nextInt(100000)
      val nextInt = rand.nextInt(3)
      if (nextInt == 0) {
        tile.foodYield = 1
      } else if (nextInt == 1) {
        tile.productionYield = 1
      } else if (nextInt == 2) {
        tile.scienceYield = 1
      }
    }

    // Unfog of war
    pieceById.values.foreach { piece =>
      piece.side match {
        case SB =>
        case (S0 | S1 | S2 | S3 | S4 | S5) =>
          setNearbyTileCityFoundingData(piece)
      }
    }

    // Set up barbarian starts
    barbarianStarts.foreach { tup =>
      val (pieceStats, startLoc) = tup;
      if (locIsValid(startLoc)) {
        spawnPieceInitial(SB,pieceStats,startLoc,startLoc,0,0,0)
        val tile = tiles(startLoc)
        if (pieceStats.name == "camp") {
          tile.foodYield = 0
          tile.productionYield = 0
          tile.scienceYield = 1
          makeAnomaliesAdjacentToHex(startLoc, 2)
        }
        if (pieceStats.name == "lair") {
          tile.foodYield = 0
          tile.productionYield = 0
          tile.scienceYield = 2
          makeAnomaliesAdjacentToHex(startLoc, 3)
        }
      }
    }

    handleStartOfTurn()
  }

  def tryGeneralLegality(action: GeneralBoardAction, externalInfo: ExternalInfo): Try[Unit] = {
    action match {
      case BuyReinforcement(pieceName,free) =>
        if(!externalInfo.pieceMap.contains(pieceName))
          Failure(new Exception("Bought reinforcement piece with unknown name: " + pieceName))
        else if(free && !canFreeBuyPiece(side,pieceName))
          Failure(new Exception("Cannot buy/carry-over this piece for free: " + pieceName))
        else
          Success(())
      case GainSpell(_) =>
          Success(())
      case AddToScienceQueue(_,_) => 
          Success(())
    }
  }

  def turnIntoAnomaly(loc: Loc): Unit = {
    val tile = tiles(loc)
    if (tile.foodYield > 0) {
      tile.foodYield = 2;
    } else if (tile.productionYield > 0) {
      tile.productionYield = 2;
    } else if (tile.scienceYield > 0) {
      tile.scienceYield = 2;
    } else {
      val rand = new Random(seed)
      seed = rand.nextInt(100000)
      val nextInt = rand.nextInt(3)
      if (nextInt == 0) {
        tile.foodYield = 2
      } else if (nextInt == 1) {
        tile.productionYield = 2
      } else if (nextInt == 1) {
        tile.scienceYield = 2
      }
    }
  }

  def makeAnomaliesAdjacentToHex(loc: Loc, numAnomalies: Int, barren: Boolean = false): Unit = {
    val rand = new Random(seed)
    seed = rand.nextInt(100000)
    val offsets = tiles.topology.adjOffsets
    val sampledOffsets = rand.shuffle(offsets).slice(0, numAnomalies)

    sampledOffsets.foreach { vec =>
      val adjLoc = loc + vec
      if (locIsValid(adjLoc)) {
        if (barren) {
          makeBarren(adjLoc)
        } else {
          turnIntoAnomaly(adjLoc)
        }
      }
    }
  }

  def makeBarren(loc: Loc): Unit = {
    val tile = tiles(loc)
    tile.foodYield = 0
    tile.productionYield = 0
    tile.scienceYield = 0
  }

  def makeAnomalyWithAdjacentBarrens(loc: Loc): Unit = {
    turnIntoAnomaly(loc)
    makeAnomaliesAdjacentToHex(loc, 2, barren=true)
  }

  def resetHasNearbyFriendlyAndEnemy(loc: Loc): Unit = {
    val tile = tiles(loc)
    Side.foreach { side => 
      tile.hasNearbyFriendly = tile.hasNearbyFriendly + (side -> false)
      tile.hasNearbyEnemy = tile.hasNearbyEnemy + (side -> false)
      tile.isVisible = tile.isVisible + (side -> false)
    }
  }

  def setNearbyTileHasNearbyFriendly(piece: Piece): Unit = {
    tiles.topology.forEachAdj(piece.loc) { loc =>
      if (locIsValid(loc)) {
        val tile = tiles(loc)
        tile.hasNearbyFriendly = tile.hasNearbyFriendly + (piece.side -> true)
      }
    }
    tiles.topology.forEachAdjRange3(piece.loc) { loc =>
      if (locIsValid(loc)) {
        val tile = tiles(loc)
        tile.isVisible = tile.isVisible + (piece.side -> true)
      }
    }
    val pieceTile = tiles(piece.loc)
    pieceTile.hasNearbyFriendly = pieceTile.hasNearbyFriendly + (piece.side -> true)
    pieceTile.isVisible = pieceTile.isVisible + (piece.side -> true)
  }

  def setNearbyTileHasNearbyEnemy(piece: Piece): Unit = {
    tiles.topology.forEachAdjRange2(piece.loc) { loc =>
      if (locIsValid(loc)) {
        val tile = tiles(loc)
        Side.foreach { side =>
          if (side != piece.side) {
            tile.hasNearbyEnemy = tile.hasNearbyEnemy + (side -> true)
          }
        }
      }
    }
    val pieceTile = tiles(piece.loc)
    Side.foreach { side =>
      if (side != piece.side) {
        pieceTile.hasNearbyEnemy = pieceTile.hasNearbyEnemy + (side -> true)
      }
    }
  }  

  def setNearbyTileCityFoundingData(piece: Piece): Unit = {
    setNearbyTileHasNearbyFriendly(piece)
    setNearbyTileHasNearbyEnemy(piece)
  }

  def canFreeBuyPiece(side: Side, pieceName: String) : Boolean = {
    allowedFreeBuyPieces(side).contains(pieceName) && numFreeBuysAllowed(side) > 0
  }
  def couldFreeBuyPieceThisTurn(side: Side, pieceName: String) : Boolean = {
    allowedFreeBuyPieces(side).contains(pieceName)
  }

  //Perform a GeneralBoardAction.
  def doGeneralBoardAction(action: GeneralBoardAction, externalInfo: ExternalInfo): Unit = {
    action match {
      case BuyReinforcement(pieceName,free) =>
        if(!externalInfo.pieceMap.contains(pieceName))
          throw new Exception("Bought reinforcement piece with unknown name: " + pieceName)
        if(free && !canFreeBuyPiece(side,pieceName))
          throw new Exception("Cannot buy/carry-over this piece for free: " + pieceName)
        if(free) {
          addReinforcementInternal(side,pieceName)
          numFreeBuysAllowed(side) -= 1
        }
        else {
          addReinforcementInternal(side,pieceName)
          val pieceStats = externalInfo.pieceMap(pieceName)
          totalCosts(side) = totalCosts(side) + pieceStats.cost
        }

      case GainSpell(spellId) =>
        spellsInHand(side) = spellsInHand(side) :+ spellId

      case AddToScienceQueue(_,_) =>
    }
  }

  //Directly spawn a piece if it possible to do so. Exposed for use to set up initial boards.
  def spawnPieceInitial(side: Side, pieceStats: PieceStats, loc: Loc): Try[Piece] = {
    trySpawnIsLegal(side, pieceStats, loc) match {
      case Failure(err) => Failure(err)
      case Success(()) =>
        val piece = spawnPieceInternal(side,pieceStats,loc).get
        Success(piece)
    }
  }

  //Directly spawn a piece if it possible to do so. Exposed for use to set up initial boards.
  def spawnPieceInitial(side: Side, pieceStats: PieceStats, loc: Loc, target: Loc, food: Double, 
    production: Double, science: Double, extraHealth: Int = 0): Try[Piece] = {

    trySpawnIsLegal(side, pieceStats, loc) match {
      case Failure(err) => Failure(err)
      case Success(()) =>
        val piece = spawnPieceInternal(side,pieceStats,loc,target,food,production,science,extraHealth=extraHealth).get
        Success(piece)
    }
  }

  //Directly add a piece to reinforcements. Exposed for use to set up initial boards.
  def addReinforcementInitial(side: Side, pieceName: String): Unit = {
    addReinforcementInternal(side,pieceName)
  }

  //Is there a piece on the current board matching this spec?
  def pieceExists(spec: PieceSpec): Boolean = {
    findPiece(spec).nonEmpty
  }

  //Find the piece, if any, matching this spec
  def findPiece(spec: PieceSpec): Option[Piece] = {
    spec match {
      case StartedTurnWithID(pieceId) =>
        pieceById.get(pieceId).flatMap { piece =>
          Some(piece)
        }
      case (spawnedThisTurn: SpawnedThisTurn) =>
        piecesSpawnedThisTurn.get(spawnedThisTurn)
    }
  }

  //pathBias - bias the search to focus on locations in this path first, so as to keep stable a path the user has traced.
  //isRotationPath - adhere to the pathBias more strongly, proceeding down it in a depth-first fashion, therefore possibly
  //returning a nonminimal path or illegal move, but allow the path to go anywhere a minimal path could reach. The purpose of this is to
  //allow the user to draw nonminimal paths to specify lines or loops along with pieces should shuffle, when they want
  //to do swaps or triangular or higher-order piece shuffles and rotations.
  private def forEachLegalMoveHelper(piece: Piece, pathBias: List[Loc], isRotationPath: Boolean)
    (f: (Loc,List[Loc],List[(PieceSpec,List[Loc])]) => Unit): Unit
  = {
    val passedThrough = scala.collection.mutable.HashSet[Loc]()
    val endedOn = scala.collection.mutable.HashSet[Loc]()

    val side = piece.side
    val pieceStats = piece.curStats(this)
    val pieceSpec = piece.spec

    val range = piece.actState match {
      case Moving(stepsUsed) => pieceStats.moveRange - stepsUsed
      case Attacking(_) | Spawning | DoneActing => 0
    }

    //Attempts to try this location as an ending location, shuffling/rotating existing friendly units on that
    //hex back along the path if already occupied (or if there would be too many, in the case of swarm).
    def tryEndingLoc(loc: Loc, revPath: List[Loc], shortestRevPath: List[Loc]): Unit = {
      if(pieces(loc).forall { other => other.side == side }) {
        var success = false
        //Make sure the original piece can walk on that tile
        if(canWalkOnTile(piece.baseStats, pieceStats,tiles(loc)) && shortestRevPath.nonEmpty) {
          //Find if shuffling works
          canShuffleOnPath(side, List(pieceStats), revPath, numMovingFromZero = 1) match {
            case Some(shuffles) =>
              f(loc, revPath, (pieceSpec,shortestRevPath) :: shuffles)
              endedOn += loc
              success = true
            case None =>
              //If the original piece was part of a swarm, try sending members of the swarm down this path as well.
              //For example, a clicking and dragging one of the bone rat in a stack of 3 to swap with a zombie should
              //perform the swap by moving the dragged rat AND the other two rats along with it.
              val otherPiecesAbleToFollow = pieces(piece.loc).filter { otherPiece =>
                otherPiece.spec != pieceSpec &&
                canMoveAtLeast(otherPiece,shortestRevPath.length-1) &&
                shortestRevPath.forall { pathLoc => canWalkOnTile(otherPiece.baseStats, otherPiece.curStats(this),tiles(pathLoc)) }
              }
              //Try each number of other pieces that could follow, and see if we get a legal shuffling
              for(numOthers <- 1 to otherPiecesAbleToFollow.length) {
                if(!success) {
                  val others = otherPiecesAbleToFollow.take(numOthers)
                  val otherStats = others.map { other => other.curStats(this) }
                  canShuffleOnPath(side, pieceStats :: otherStats, revPath, numMovingFromZero = 1 + numOthers) match {
                    case Some(shuffles) =>
                      val otherMoves = others.map { other => (other.spec,shortestRevPath) }
                      f(loc, revPath, (pieceSpec,shortestRevPath) :: (otherMoves ++ shuffles))
                      endedOn += loc
                      success = true
                    case None => ()
                  }
                }
              }
          }
        }
        //If not successsful, then call f anyways with an empty move list
        if(!success)
          f(loc, revPath, List())
      }
    }

    //If rotating pieces, then first find the set of all locations we can actually reach and minimal paths for reaching them.
    //That way, we can allow nonminimal rotation paths, but have the moving piece actually take the shortest path to reach
    //the target loc, rather than the nonminimal path.
    var shortestRevPathToLoc: Map[Loc,List[Loc]] = Map()
    if(isRotationPath) {
      val seen = scala.collection.mutable.HashSet[Loc]()
      //Breadth first floodfill
      var q = Vector[(Loc, Int, List[Loc])]()
      q = q :+ ((piece.loc, 0, List(piece.loc)))
      while(!q.isEmpty) {
        val (loc,d,revPath) = q(0)
        q = q.drop(1)
        if(!seen.contains(loc)) {
          seen += loc
          shortestRevPathToLoc = shortestRevPathToLoc + (loc -> revPath)

          if(d < range) {
            topology.forEachAdj(loc) { y =>
              //Test boundary and also ensure path cannot collide with itself
              if(tiles.inBounds(y) && !revPath.contains(y) && canMoveThroughLoc(piece,y))
                q = q :+ ((y, d+1, y :: revPath))
            }
          }
        }
      }
    }


    //Breadth first floodfill
    var q = Vector[(Loc, Int, List[Loc], List[Loc])]()
    q = q :+ ((piece.loc, 0, List(piece.loc), List(piece.loc)))
    while(!q.isEmpty) {
      val (loc,d,revPath,shortestRevPath) = q(0)
      q = q.drop(1)

      //Test boundary and also ensure path cannot collide with itself
      def isNextLocOk(y: Loc): Boolean = {
        tiles.inBounds(y) && !revPath.contains(y) && canMoveThroughLoc(piece,y)
      }

      if(!endedOn.contains(loc))
        tryEndingLoc(loc,revPath,shortestRevPath)

      if(!passedThrough.contains(loc)) {
        passedThrough += loc

        //Proceed depth-first when doing a rotation path, to allow nonminimal paths.
        if(isRotationPath) {
          //Find the location that follows the current location in the given path bias, if any
          val idx = pathBias.indexOf(loc)
          if(idx >= 0 && idx < pathBias.length - 1) {
            val y = pathBias(idx+1)
            if(topology.distance(loc,y) == 1 && isNextLocOk(y)) {
              //And make sure we have a shortest path to reach it
              shortestRevPathToLoc.get(y) match {
                case None =>
                  //Can't actually end with a rotation on this hex, but perhaps the rotation
                  //goes further and comes back into range, so we add it to the queue.
                  q = ((y, d+1, y :: revPath, List())) +: q
                case Some(newShortestRevPath) =>
                  q = ((y, newShortestRevPath.length-1, y :: revPath, newShortestRevPath)) +: q
              }
            }
          }
        }

        //Otherwise respect normal movement range
        if(d < range) {
          //Enqueue locations that we're biased to prefer first, so that we try those paths first.
          pathBias.foreach { y =>
            if(topology.distance(loc,y) == 1)
              if(isNextLocOk(y))
                q = q :+ ((y, d+1, y :: revPath, y :: shortestRevPath))
          }
          //Then enqueue all other locations
          topology.forEachAdj(loc) { y =>
            if(!pathBias.contains(y) && isNextLocOk(y))
              q = q :+ ((y, d+1, y :: revPath, y :: shortestRevPath))
          }
        }
      }
    }
  }

  //Find the set of all legal locations that a piece can move to, along with the number of steps to reach the location.
  //Does not include teleports.
  def legalMoves(piece : Piece): Map[Loc, Int] = {
    val ans = scala.collection.mutable.HashMap[Loc, Int]()
    forEachLegalMoveHelper(piece,List(),isRotationPath=false) { case (loc,revPath,revMovements) =>
      if(revMovements.nonEmpty)
        ans(loc) = revPath.length
    }
    Map() ++ ans
  }

  def withinTerrainRange(piece : Piece, steps : Int) : Set[Loc] = {
    var ans = Set[Loc]()
    tiles.topology.forEachReachable(piece.loc) { (loc,dist) =>
      if(dist<=steps && inBounds(loc) && canWalkOnTile(piece.baseStats, piece.curStats(this), tiles(loc))) {
        ans = ans + loc
        dist < steps //Can continue moving from this location
      } else {
        false //Can't keep moving from this location
      }
    }
    ans
  }

  def attackableHexes(piece: Piece) : Set[Loc] = {
    val stats = piece.curStats(this)
    val moveLocs = withinTerrainRange(piece, stats.moveRange)
    val moveLocsPieceCouldAttackFrom = { if(stats.isLumbering) Set(piece.loc) else moveLocs }
    var attackLocs = Set[Loc]()
    moveLocsPieceCouldAttackFrom.foreach { fromLoc =>
      tiles.topology.forEachReachable(fromLoc) { (loc,dist) =>
        if(dist<=stats.attackRange && inBounds(loc)) {
          attackLocs += loc
          dist < stats.attackRange
        } else {
          false
        }
      }
    }
    attackLocs
  }

  def countGraveyards(side: Side) : Int = {
    var ans = 0
    tiles.foreachi { case (loc,tile) =>
      if(tile.terrain == Graveyard && pieceById.values.exists { piece => piece.side == side && piece.loc == loc }) {
        ans += 1
      }
    }
    return ans
  }

  def potentiallyThreatened(piece: Piece) : Boolean = {
    assert(piece.baseStats.isNecromancer)
    var potentialDamage = 0
    pieceById.values.foreach { enemy =>
      if(enemy.side != piece.side) {
        if(attackableHexes(enemy).contains(piece.loc)) {
          enemy.curStats(this).attackEffect match {
            case Some(Damage(n)) =>
              potentialDamage += n*enemy.curStats(this).numAttacks
            case _ => ()
          }
        }
      }
    }
    piece.curStats(this).defense match {
      case Some(hp) => hp <= potentialDamage
      case None => false
    }
  }

  //Similar to legalMoves but finds a path whose destination satisfies the desired predicate,
  //along with list of pieces and paths to move (usually one, but multiple in the case of swaps or rotations)
  //May return a nonminimal path in the case that the path is traversing friendly units such that the user could
  //be trying to perform a swap or rotation.
  //Does not include teleports.
  //Might return an illegal move if isRotationPath is true, in that case the movements list will be empty.
  def findPathForUI(piece : Piece, pathBias: List[Loc], isRotationPath: Boolean)
    (f: (Loc,List[(PieceSpec,List[Loc])]) => Boolean): Option[(List[Loc],List[(PieceSpec,List[Loc])])]
  = {
    forEachLegalMoveHelper(piece,pathBias,isRotationPath) { case (loc,revPath,revMovements) =>
      if(f(loc,revMovements)) {
        val movements = revMovements.map { case (spec,rpath) => (spec, rpath.reverse) }
        return Some((revPath.reverse, movements))
      }
    }
    None
  }

  //Find all locations that a piece can legally spawn
  def legalSpawnLocs(spawnStats: PieceStats): List[Loc] = {
    //Not the most efficient algorithm to just iterate and test every loc, but it should work
    tiles.filterLocs { loc =>
      //Make up a fake spec that won't match anything else
      val pieceSpec = SpawnedThisTurn(spawnStats.name, loc, nthAtLoc = -1)
      tryCanEndOnLoc(side, pieceSpec, spawnStats, spawnStats, loc, Nil).isSuccess &&
      isSpawnerInRange(loc,spawnStats)
    }
  }

  def canAttack(attackerStats: PieceStats, attackerHasMoved: Boolean, attackerState: ActState, targetStats: PieceStats): Boolean = {
    tryCanAttack(attackerStats, attackerHasMoved, attackerState, targetStats).isSuccess
  }

  def tryCanAttack(attackerStats: PieceStats, attackerHasMoved: Boolean, attackerState: ActState, targetStats: PieceStats): Try[Unit] = {
    val attackRange = { if(targetStats.isFlying) attackerStats.attackRangeVsFlying else attackerStats.attackRange }
    if(attackerStats.attackEffect.isEmpty || attackerStats.numAttacks == 0) failed("Piece cannot attack")
    else if(attackRange <= 0) failed("Piece cannot attack this unit")
    else if(attackerStats.isLumbering && attackerHasMoved) failed("Lumbering pieces cannot both move and attack on the same turn")
    else {
      val result = attackerState match {
        case Moving(_) => Success(())
        case Attacking(numAttacks) =>
          //Slightly more specific err message
          if(attackerStats.numAttacks == 1 && numAttacks > 0) failed("Piece already attacked")
          //Fuller check
          else if(numAttacks >= attackerStats.numAttacks) failed("Piece already assigned all of its attacks")
          else Success(())
        case Spawning | DoneActing =>
          failed("Piece has already acted or cannot attack this turn")
      }
      result.flatMap { case () =>
        val result2 = attackerStats.attackEffect match {
          case None => failed("Piece cannot attack")
          case Some(Damage(_)) => Success(())
          case Some(Kill) =>
            if(targetStats.isNecromancer) failed("Death attacks cannot hurt necromancers")
            else Success(())
          case Some(Unsummon) =>
            if(targetStats.isPersistent) failed("Target is persistent - cannot be unsummoned")
            else Success(())
          case Some(Enchant(_)) => Success(())
          case Some(TransformInto(_)) =>
            if(targetStats.isNecromancer) failed("Necromancers cannot be transformed")
            else Success(())
        }
        result2.flatMap { case () =>
          if(!attackerStats.canHurtNecromancer && targetStats.isNecromancer) failed("Piece cannot hurt necromancer")
          else Success(())
        }
      }
    }
  }

  def inBounds(loc: Loc): Boolean = {
    tiles.inBounds(loc)
  }
  def canWalkOnTile(baseStats: PieceStats, pieceStats: PieceStats, tile: Tile): Boolean = {
    tryCanWalkOnTile(baseStats, pieceStats, tile).isSuccess
  }

  //HELPER FUNCTIONS -------------------------------------------------------------------------------------
  private def tryCanWalkOnTile(baseStats: PieceStats, pieceStats: PieceStats, tile: Tile): Try[Unit] = {
    tile.terrain match {
      case Wall => failed("Cannot move or spawn through borders")
      case Ground | Graveyard | SorceryNode | Teleporter |  Spawner(_) | Mist => Success(())
      case Water(_) => if(pieceStats.isFlying) Success(()) else failed("Non-flying pieces cannot move or spawn on water")
      case Earthquake(_) =>
        if(baseStats.moveRange >= 2) Success(()) else failed("Only unit types with at least two speed can move through an earthquake")
      case Firestorm(_) =>
        if(baseStats.defense.getOrElse(4) >= 4) Success(()) else failed("Only unit types with at least four health can move through a firestorm")
      case Whirlwind(_) =>
        if(baseStats.isPersistent) Success(()) else failed("Only persistent unit types can move through a whirlwind")
    }
  }

  private def canMoveThroughLoc(piece: Piece, loc: Loc): Boolean = {
    val pieceStats = piece.curStats(this)
    canWalkOnTile(piece.baseStats, pieceStats,tiles(loc)) &&
    (pieceStats.isFlying || pieces(loc).forall { other => other.side == piece.side })
  }
  private def tryCanMoveThroughLoc(piece: Piece, loc: Loc): Try[Unit] = {
    val pieceStats = piece.curStats(this)
    tryCanWalkOnTile(piece.baseStats, pieceStats,tiles(loc)).flatMap { case () =>
      if(pieceStats.isFlying || pieces(loc).forall { other => other.side == piece.side }) Success(())
      else failed("Non-flying pieces cannot move through enemies")
    }
  }

  def canSwarmTogether(pieceStats: PieceStats, otherStats: PieceStats): Boolean = {
    pieceStats.swarmMax > 1 && otherStats.swarmMax > 1 && pieceStats.name == otherStats.name
  }

  private def canSwarmToLoc(pieceSpec: PieceSpec, pieceStats: PieceStats, loc: Loc, simultaneousMovements: List[Movement]): Boolean = {
    var piecesOnFinalSquare = 1
    var minSwarmMax = pieceStats.swarmMax
    def pieceIfMovingTo(pieceSpec: PieceSpec, path: Vector[Loc], dest: Loc): Option[Piece] =
      if(path.length > 0 && path.last == dest) findPiece(pieceSpec)
      else None
    def okSwarmer(otherStats: PieceStats): Boolean = {
      if(!canSwarmTogether(otherStats,pieceStats)) false
      else {
        piecesOnFinalSquare += 1
        minSwarmMax = Math.min(minSwarmMax,otherStats.swarmMax)
        true
      }
    }
    val ok = {
      pieces(loc).forall { other =>
        if(other.spec == pieceSpec) true
        else if(simultaneousMovements.exists { case Movement(spec,_) => other.spec == spec }) true
        else okSwarmer(other.curStats(this))
      } && simultaneousMovements.forall { case Movement(otherSpec,otherPath) =>
          if(pieceSpec == otherSpec) true
          else pieceIfMovingTo(otherSpec,otherPath,loc).forall { other => okSwarmer(other.curStats(this)) }
      }
    }
    ok && piecesOnFinalSquare <= minSwarmMax
  }

  private def canEndOnLoc(side: Side, pieceSpec: PieceSpec, baseStats: PieceStats, pieceStats: PieceStats, loc: Loc, simultaneousMovements: List[Movement]): Boolean = {
    tiles.inBounds(loc) &&
    canWalkOnTile(baseStats, pieceStats,tiles(loc)) &&
    pieces(loc).forall { other => other.side == side } &&
    canSwarmToLoc(pieceSpec,pieceStats,loc,simultaneousMovements)
  }
  def tryCanEndOnLoc(side: Side, pieceSpec: PieceSpec, baseStats: PieceStats, pieceStats: PieceStats, loc: Loc, simultaneousMovements: List[Movement]): Try[Unit] = {
    if(!tiles.inBounds(loc)) failed("Location not in bounds")
    else {
      tryCanWalkOnTile(baseStats, pieceStats,tiles(loc)).flatMap { case () =>
        if(!pieces(loc).forall { other => other.side == side }) failed("Piece would end in the same space as enemies")
        else {
          if(!canSwarmToLoc(pieceSpec,pieceStats,loc,simultaneousMovements)) {
            if(pieceStats.swarmMax > 1) failed("Pieces cannot swarm together or too many in the same space")
            else failed("Piece would end in same space as other pieces")
          }
          else Success(())
        }
      }
    }
  }

  private def canMoveAtLeast(piece: Piece, steps: Int): Boolean = {
    piece.actState match {
      case Moving(stepsUsed) => piece.curStats(this).moveRange >= stepsUsed + steps
      case Attacking(_) | Spawning | DoneActing => false
    }
  }

  //Is it possible for these pieces to end on this location, where any friendly pieces in the way
  //that can move get to shuffle back along the path?
  //numMovingFromZero is the number of pieces moving out of the starting hex on the path simultaneously.
  //Returns all the shuffles needed if possible. Paths returned are reversed.
  //Precondition: Except for restrictions on friendly pieces/swarming on the final square, it's legal for
  //all the pieces to move along this path AND they have enough moverange to do so.
  private def canShuffleOnPath(side: Side, pieceStatss: List[PieceStats],
    revPath: List[Loc], numMovingFromZero: Int): Option[List[(PieceSpec,List[Loc])]]
  = {
    val loc = revPath.head
    val remainingPath = revPath.tail
    //If we can simply move there, then we're done
    if(pieces(loc).isEmpty || (remainingPath.length == 0 && numMovingFromZero == pieces(loc).length))
      Some(List())
    else {
      //Else it's occupied by friendly pieces. Compute the excess number of friendly pieces based on swarm limits.
      val excess = {
        //If they can't swarm, then the excess is all of them.
        if(!pieceStatss.forall { pieceStats => pieces(loc).forall { otherStats => canSwarmTogether(otherStats.curStats(this),pieceStats) } })
          pieces(loc).length
        //Otherwise it's how much they exceed the max.
        else
          pieces(loc).length + pieceStatss.length - pieceStatss.head.swarmMax
      }
      //If there's no excess, then we're good
      if(excess <= { if(remainingPath.length == 0) numMovingFromZero else 0 })
        Some(List())
      //Otherwise, if that's the end of the path, then we're stuck.
      else if(remainingPath.length == 0)
        None
      else {
        //Okay, shuffle down one hex. The next hex in the path must be friendly-occupied or empty.
        val nextLoc = remainingPath.head
        if(!pieces(nextLoc).forall { other => other.side == side })
          None
        else {
          //Filter to the pieces able to shuffle down, and make sure enough can.
          val piecesAbleToShuffle = pieces(loc).filter { piece =>
            canMoveAtLeast(piece,1) && canWalkOnTile(piece.baseStats, piece.curStats(this),tiles(nextLoc))
          }
          if(piecesAbleToShuffle.length < excess)
            None
          else {
            //And recurse!
            val piecesMoving = piecesAbleToShuffle.take(excess)
            val nextStatss = piecesMoving.map { piece => piece.curStats(this) }
            canShuffleOnPath(side, nextStatss, remainingPath, numMovingFromZero) match {
              case None => None
              case Some(shuffles) =>
                Some(piecesMoving.map { piece => (piece.spec,List(nextLoc,loc)) } ++ shuffles)
            }
          }
        }
      }
    }
  }

  private def isSpawnerInRange(spawnLoc: Loc, spawnStats:PieceStats): Boolean = {
    pieceById.values.exists { piece =>
      val distance = topology.distance(spawnLoc,piece.loc)
      if(piece.side != side) false
      else {
        val spawnerStats = piece.curStats(this)
        if((!spawnStats.isEldritch || distance > 1) && (spawnerStats.spawnRange.forall { d => d < distance })) false
        else if(spawnerStats.isWailing) false
        else if(piece.actState >= DoneActing) false
        else true
      }
    }
  }

  def canMoveTerrain(loc: Loc) : Try[Unit] = {
    if(pieces(loc).nonEmpty) Failure(new Exception("Target location is not empty"))
    else if(tiles(loc).terrain != Ground) Failure(new Exception("Target terrain is not Ground"))
    else {
      val pieceInRange =
        pieceById.values.exists { piece =>
          val distance = topology.distance(loc, piece.loc)
          piece.actState < DoneActing && distance <= 1 && piece.side == side
        }
      if(!pieceInRange) Failure(new Exception("No adjacent friendly piece"))
      else Success(())
    }
  }

  def moveTerrain(terrain: Terrain, loc: Loc) = {
    tiles.transform { tile =>
      if(tile.terrain == terrain) {
        Tile(Ground, tile.startingTerrain, modsWithDuration = tile.modsWithDuration,0,0,0,0,0,0,Map(),Map(),Map())
      } else {
        tile
      }
    }
    tiles(loc) = Tile(terrain, tiles(loc).startingTerrain, modsWithDuration = tiles(loc).modsWithDuration,0,0,0,0,0,0,Map(),Map(),Map())
  }

  private def killAttackingWailingUnits(externalInfo: ExternalInfo, otherThan: Option[PieceSpec] = None): Unit = {
    val attackedWailings = pieceById.iterator.filter { case (_,piece) =>
      piece.curStats(this).isWailing && otherThan.forall { spec => piece.spec != spec }
    }
    attackedWailings.toList.foreach { case (_,piece) => killPiece(piece, externalInfo) }
  }

  private def trySpellTargetLegality(spell: Spell, targets: SpellOrAbilityTargets): Try[Unit] = {
    spell match {
      case (spell: TargetedSpell) =>
        findPiece(targets.target0) match {
          case None => failed("No target specified for spell")
          case Some(target) => spell.tryCanTarget(side,target,this)
        }
      case (spell: TileSpell) =>
        if(!tiles.inBounds(targets.loc0)) failed("Target location not in bounds")
        else spell.tryCanTarget(side,targets.loc0,this)
      case (spell: PieceAndLocSpell) =>
        findPiece(targets.target0) match {
          case None => failed("No target specified for spell")
          case Some(target) =>
            if(!tiles.inBounds(targets.loc0)) failed("Target location not in bounds")
            else spell.tryCanTarget(side,target,targets.loc0,this)
        }
      case (spell: TerrainAndTileSpell) =>
        if(targets.terrain.isEmpty) failed("No target specified for spell")
        if(!tiles.inBounds(targets.loc0)) failed("Target location not in bounds")
        spell.tryCanTarget(side,targets.terrain.get,targets.loc0,this)
      case (_: NoTargetSpell) =>
        Success(())
    }
  }

  private def manaOfDiscard(spellType: SpellType): Int = {
    spellType match {
      case NormalSpell => 1
      case Sorcery => 1
      case Cantrip => 1
      case DoubleCantrip => 2
    }
  }

  private def manaOfPlay(spellType: SpellType): Int = {
    spellType match {
      case NormalSpell => 0
      case Sorcery => -1
      case Cantrip => 1
      case DoubleCantrip => 2
    }
  }

  private def manaInHand(side: Side, externalInfo: ExternalInfo, excludingSpell: Option[SpellId] = None): Int = {
    spellsInHand(side).foldLeft(0) { case (power,spellId) =>
      if(excludingSpell == Some(spellId))
        power
      else {
        externalInfo.spellsRevealed.get(spellId) match {
          //Assume that unknown spells are worth 2 mana when discarded so that we
          //don't complain about move legality from the opponent when we don't know their hand
          case None => power + 2
          case Some(spellName) =>
            val spell = Spells.spellMap(spellName)
            power + manaOfDiscard(spell.spellType)
        }
      }
    }
  }

  //Tests if a spawn is possible. Doesn't test reinforcements (since other things can
  //cause spawns, such as death spawns, this functions handles the most general tests).
  private def spawnIsLegal(spawnSide: Side, spawnStats: PieceStats, spawnLoc: Loc): Boolean = {
    //Make up a fake spec that won't match anything else
    val pieceSpec = SpawnedThisTurn(spawnStats.name, spawnLoc, nthAtLoc = -1)
    canEndOnLoc(spawnSide, pieceSpec, spawnStats, spawnStats, spawnLoc, Nil)
  }
  def trySpawnIsLegal(spawnSide: Side, spawnStats: PieceStats, spawnLoc: Loc): Try[Unit] = {
    //Make up a fake spec that won't match anything else
    val pieceSpec = SpawnedThisTurn(spawnStats.name, spawnLoc, nthAtLoc = -1)
    tryCanEndOnLoc(spawnSide, pieceSpec, spawnStats, spawnStats, spawnLoc, Nil)
  }

  //Check if a single action is legal
  private def tryLegalitySingle(action: PlayerAction, externalInfo: ExternalInfo): Try[Unit] = Try {
    failIf(turnNumber < 0, "Game is not started yet")
    failIf(!canMove, "After you win on graveyards, your opponent gets the first turn")
    action match {
      case Movements(movements) =>
        //Check basic properties of the set of movements
        val pieceIdsMoved = movements.map { case Movement(pieceId,_) => pieceId }
        failIf(pieceIdsMoved.distinct.length != movements.length, "More than one movement for the same piece")
        failIf(movements.isEmpty, "Empty set of movements")

        movements.foreach { case Movement(pieceSpec,path) =>
          //Check basic properties of path
          failUnless(path.length > 1, "Empty or trivial movement path")
          failUnless(path(0) != path.last, "Circular movement path")
          failUnless(path.forall { loc => tiles.inBounds(loc) }, "Movement out of bounds")
          failUnless((1 to (path.length - 1)).forall { idx => topology.distance(path(idx-1),path(idx)) == 1 },
            "Movement path locations not all adjacent")

          findPiece(pieceSpec) match {
            case None => fail("Moving a nonexistent or dead piece")
            case Some(piece) =>
              //Ownership
              failUnless(piece.side == side, "Piece controlled by other side")

              val pieceStats = piece.curStats(this)

              //Piece movement range and state is ok
              failIf(pieceStats.moveRange <= 0, "Piece cannot move")
              piece.actState match {
                case Moving(stepsUsed) =>
                  failUnless(path.length - 1 <= pieceStats.moveRange - stepsUsed, "Movement range of piece is not large enough")
                case Attacking(_) | Spawning | DoneActing =>
                  fail("Piece has already acted or cannot move this turn")
              }

              //Piece currently at the start of the path
              failUnless(piece.loc == path(0), "Moved piece is not at the head of the path")
              //Check spaces along the path
              path.foreach { loc => tryCanMoveThroughLoc(piece, loc).get}
              //Check space at the end of the path
              tryCanEndOnLoc(piece.side, piece.spec, piece.baseStats, pieceStats, path.last, movements).get
          }
        }

      case Attack(attackerSpec, targetSpec) =>
        (findPiece(attackerSpec),findPiece(targetSpec)) match {
          case (None, _) => fail("Attacking with a nonexistent or dead piece")
          case (Some(_),None) => fail("Attacking a nonexistent or dead piece")
          case (Some(attacker),Some(target)) =>
            failUnless(attacker.side == side, "Attacker is controlled by other side")
            failUnless(target.side != side, "Target piece is friendly")
            failUnless(tiles.inBounds(target.loc), "Target location not in bounds")
            val attackerStats = attacker.curStats(this)
            val targetStats = target.curStats(this)
            val attackRange = { if(targetStats.isFlying) attackerStats.attackRangeVsFlying else attackerStats.attackRange }
            failUnless(topology.distance(attacker.loc,target.loc) <= attackRange, "Attack range not large enough")
            tryCanAttack(attackerStats, attacker.hasMoved, attacker.actState, targetStats).get
        }

      case Spawn(spawnLoc, spawnName) =>
        //A bunch of tests that don't depend on the spawner or on reinforcements state
        val spawnStats = externalInfo.pieceMap(spawnName)
        trySpawnIsLegal(side, spawnStats, spawnLoc).get
        failUnless(isSpawnerInRange(spawnLoc,spawnStats), "No non-newly-spawned piece with spawn in range")
        failUnless(reinforcements(side).contains(spawnName), "No such piece in reinforcements")

      case ActivateTile(loc) =>
        failUnless(tiles.inBounds(loc), "Activated location not in bounds")
        tiles(loc).terrain match {
          case Wall | Ground | Water(_) | Graveyard | SorceryNode | Teleporter |
               Earthquake(_) | Firestorm(_) | Whirlwind(_) | Mist =>
            fail("Tile cannot be activated")
          case Spawner(spawnName) =>
            failIf(pieces(loc).nonEmpty, "Spawner tile must be unoccupied")
            failIf(hasUsedSpawnerTile, "Already used a spawner tile this turn")
            trySpawnIsLegal(side, externalInfo.pieceMap(spawnName), loc).get
        }

      case ActivateAbility(pieceSpec,ability,targets) =>
        findPiece(pieceSpec) match {
          case None => fail("Using ability of a nonexistent or dead piece")
          case Some(piece) =>
            failUnless(piece.side == side, "Piece controlled by other side")
            failIf(piece.actState >= DoneActing, "Piece has already acted or cannot act this turn")
            requireSuccess(ability.tryIsUsableNow(piece))
            failIf(ability.isSorcery && mana + manaInHand(side,externalInfo) <= 0, "No mana (must first play cantrip or discard spell)")
            ability match {
              case Suicide | SpawnZombies | NecroPickAbility | (_:SelfEnchantAbility) => ()
              case KillAdjacent =>
                failIf(piece.actState >= Spawning, "Piece has already acted or cannot act this turn")
              case MoveTerrain | MoveEarthquake | MoveFlood | MoveWhirlwind | MoveFirestorm  =>
                failUnless(topology.distance(piece.loc,targets.loc0) <= 1, "Must place terrain in an adjacent hex")
                requireSuccess(canMoveTerrain(targets.loc0))
              case (ability:TargetedAbility) =>
                findPiece(targets.target0) match {
                  case None => fail("No target specified for ability")
                  case Some(target) => requireSuccess(ability.tryCanTarget(piece,target))
                }
            }
        }

      case Blink(pieceSpec,_) =>
        findPiece(pieceSpec) match {
          case None => fail("Blinking a nonexistent or dead piece")
          case Some(piece) =>
            failUnless(piece.side == side, "Piece controlled by other side")
            failUnless(piece.curStats(this).canBlink, "Piece cannot blink")
        }

      case Teleport(pieceSpec,src,dest) =>
        failUnless(tiles.inBounds(src), "Teleport source out of bounds")
        failUnless(tiles.inBounds(dest), "Teleport destination out of bounds")
        failUnless(tiles(src).terrain == Teleporter, "Must be at a teleporter to teleport")

        findPiece(pieceSpec) match {
          case None => fail("Moving a nonexistent or dead piece")
          case Some(piece) =>
            failUnless(piece.side == side, "Piece controlled by other side")
            failUnless(piece.loc == src, "Teleporting piece is not on the teleporter")
            piece.actState match {
              case Moving(stepsUsed) =>
                failUnless(stepsUsed == 0, "Piece must start turn on teleporter without moving or acting")
              case Attacking(_) | Spawning | DoneActing =>
                fail("Piece must start turn on teleporter without moving or acting")
            }
            val pieceStats = piece.curStats(this)
            tryCanEndOnLoc(piece.side, piece.spec, piece.baseStats, pieceStats, dest, List()).get
        }

      case PlaySpell(spellId,targets) =>
        spellsInHand(side).contains(spellId) match {
          case false => fail("Spell not in hand or already played or discarded")
          case true =>
            externalInfo.spellsRevealed.get(spellId) match {
              case None => fail("Spell was not revealed: spellId="+spellId)
              case Some(spellName) =>
                Spells.spellMap.get(spellName) match {
                  case None => fail("Unknown spell name")
                  case Some(spell)  =>
                    val manaAfter = mana + manaInHand(side, externalInfo, excludingSpell=Some(spellId)) + manaOfPlay(spell.spellType)
                    failIf(manaAfter < 0, "Not enough mana")
                    trySpellTargetLegality(spell,targets).get
                }
            }
        }

      case DiscardSpell(spellId) =>
        spellsInHand(side).contains(spellId) match {
          case false => fail("Spell not in hand or already played or discarded")
          case true => ()
        }
      case AddToQueue(_,_,_) => 
        ()
      case ClearQueue(_,_,_) =>
        ()
      case SetTarget(_,_) =>
        ()
      case SetFocus(_,_) =>
        ()
      case PieceSuicide(_) =>
        ()
      case FoundCity(_,_) =>
        ()
    }
  }

  //Perform a single action, doing nothing if it isn't legal
  private def doActionSingle(action:PlayerAction, externalInfo: ExternalInfo): Try[Unit] = {
    tryLegalitySingle(action,externalInfo).map { case () => doActionSingleAssumeLegal(action,externalInfo) }
  }

  def doMovePieceToLoc(piece: Piece, dest: Loc): Unit = {
    val src = piece.loc
    pieces(src) = pieces(src).filterNot { p => p.id == piece.id }
    pieces(dest) = pieces(dest) :+ piece
    piece.loc = dest
  }

  def isBarbarianEncampment(stats: PieceStats): Boolean = {
    return stats.name == "camp" || stats.name == "lair"
  }

  private def doActionSingleAssumeLegal(action:PlayerAction, externalInfo: ExternalInfo): Unit = {
    action match {
      case Movements(movements) =>
        movements.foreach { movement =>
          val piece = findPiece(movement.pieceSpec).get
          val dest = movement.path.last
          doMovePieceToLoc(piece,dest)
          piece.hasMoved = true
          piece.actState = piece.actState match {
            case Moving(n) => Moving(n + movement.path.length - 1)
            case Attacking(_) | Spawning | DoneActing => assertUnreachable()
          }
        }
      case Attack(attackerSpec, targetSpec) =>
        val attacker = findPiece(attackerSpec).get
        val target = findPiece(targetSpec).get
        val attackerStats = attacker.curStats(this)
        val attackEffect = attackerStats.attackEffect.get
        applyEffect(attackEffect,target,externalInfo)
        attacker.actState = attacker.actState match {
          case Moving(_) => Attacking(1)
          case Attacking(n) => Attacking(n+1)
          case Spawning | DoneActing => assertUnreachable()
        }

        if(attackerStats.isWailing) {
          attacker.actState match {
            case Moving(_) | Spawning | DoneActing => assertUnreachable()
            case Attacking(numAttacks) =>
              if(numAttacks >= attackerStats.numAttacks)
                killPiece(attacker,externalInfo)
          }
        }

      case Spawn(spawnLoc, spawnName) =>
        val spawnStats = externalInfo.pieceMap(spawnName)
        spawnPieceInternal(side,spawnStats,spawnLoc) match {
          case Some(_: Piece) => ()
          case None => assertUnreachable()
        }
        reinforcements(side) = {
          reinforcements(side).get(spawnName) match {
            case None => assertUnreachable()
            case Some(n) =>
              if(n <= 1) reinforcements(side) - spawnName
              else reinforcements(side) + (spawnName -> (n-1))
          }
        }

      case ActivateTile(loc) =>
        tiles(loc).terrain match {
          case Wall | Ground | Water(_) | Graveyard | SorceryNode | Teleporter |
               Earthquake(_) | Firestorm(_) | Whirlwind(_) | Mist =>
            assertUnreachable()
          case Spawner(spawnName) =>
            hasUsedSpawnerTile = true
            spawnPieceInternal(side,externalInfo.pieceMap(spawnName),loc) match {
              case Some(_: Piece) => ()
              case None => assertUnreachable()
            }
        }

      case ActivateAbility(pieceSpec,ability,targets) =>
        val piece = findPiece(pieceSpec).get
        if(ability.isSorcery)
          mana -= 1

        ability match {
          case Suicide =>
            killPiece(piece,externalInfo)
          case SpawnZombies =>
            pieces.topology.forEachAdj(piece.loc) { loc =>
              spawnPieceInternal(piece.side,Units.zombie,loc) match {
                case Some(_: Piece) => ()
                case None => ()
              }
            }
          case NecroPickAbility =>
            assert(allowedNecros(piece.side).size > 1)
            // FIXME remove all necromancers from board? from hex? would currently break with spawn necros.
            removeFromBoard(piece)
            val necroIdx = allowedNecros(piece.side).indexOf(piece.baseStats.name)
            val newNecroName = allowedNecros(piece.side)((necroIdx+1)%allowedNecros(piece.side).size)
            val newNecroStats = externalInfo.pieceMap(newNecroName)
            spawnPieceInitial(piece.side,newNecroStats,piece.loc) match {
              case Success(newNecro: Piece) =>
                val necroPick = PieceModWithDuration(PieceMods.NecroPick, turnsLeft=None)
                newNecro.modsWithDuration = newNecro.modsWithDuration :+ necroPick
              case Failure(_) => assertUnreachable()
            }
          case KillAdjacent =>
            pieces.topology.forEachAdj(piece.loc) { loc =>
              pieces(loc).foreach { p =>
                if(p.side != piece.side && !p.baseStats.isNecromancer) {
                  killPiece(p,externalInfo)
                }
              }
            }
          case MoveTerrain =>
            moveTerrain(targets.terrain.get, targets.loc0)
          case MoveEarthquake =>
            moveTerrain(Earthquake(true), targets.loc0)
          case MoveFlood =>
            moveTerrain(Water(true), targets.loc0)
          case MoveWhirlwind =>
            moveTerrain(Whirlwind(true), targets.loc0)
          case MoveFirestorm =>
            moveTerrain(Firestorm(true), targets.loc0)
          case (ability:SelfEnchantAbility) =>
            piece.modsWithDuration = piece.modsWithDuration :+ ability.mod
          case (ability:TargetedAbility) =>
            val target = findPiece(targets.target0).get
            applyEffect(ability.effect,target,externalInfo)
        }

      case Blink(pieceSpec,_) =>
        val piece = findPiece(pieceSpec).get
        unsummonPiece(piece)

      case Teleport(pieceSpec,src,dest) =>
        val piece = findPiece(pieceSpec).get
        pieces(src) = pieces(src).filterNot { p => p.id == piece.id }
        pieces(dest) = pieces(dest) :+ piece
        piece.loc = dest
        piece.actState = DoneActing

      case PlaySpell(spellId,targets) =>
        val spell = Spells.spellMap(externalInfo.spellsRevealed(spellId))
        spell.spellType match {
          case NormalSpell => ()
          case Sorcery => mana -= 1
          case Cantrip => mana += 1
          case DoubleCantrip => mana += 2
        }

        spell match {
          case (spell: TargetedSpell) =>
            val target = findPiece(targets.target0).get
            applyEffect(spell.effect,target,externalInfo)
          case (spell: TileSpell) =>
            spell.effect(this,targets.loc0)
            val piecesOnTile = pieces(targets.loc0)
            piecesOnTile.foreach { piece => killIfEnoughDamage(piece, externalInfo)}
          case (spell: PieceAndLocSpell) =>
            val target = findPiece(targets.target0).get
            spell.effect(this,target,targets.loc0)
          case (spell: TerrainAndTileSpell) =>
            spell.effect(this,targets.terrain.get,targets.loc0)
          case (spell: NoTargetSpell) => spell.effect(this, side)
        }
        spellsInHand(side) = spellsInHand(side).filterNot { i => i == spellId }
        spellsPlayed = spellsPlayed :+ SpellPlayedInfo(spellId, side, Some(targets))

      case DiscardSpell(spellId) =>
        val spell = Spells.spellMap(externalInfo.spellsRevealed(spellId))
        mana += manaOfDiscard(spell.spellType)
        spellsInHand(side) = spellsInHand(side).filterNot { i => i == spellId }
        spellsPlayed = spellsPlayed :+ SpellPlayedInfo(spellId, side, None)
      case AddToQueue(pieceName, selectedCityId, isScience) => 
        val selectedCity = pieceById(selectedCityId)
        if (isScience) {
          if (isWonder(pieceName)) {
            if (!wonderBuilt(pieceName)) {
              selectedCity.scienceQueue = selectedCity.scienceQueue :+ externalInfo.pieceMap(pieceName)
            }
          }
          else if (pieceName != "warrior") {
            selectedCity.scienceQueue = selectedCity.scienceQueue :+ externalInfo.pieceMap(pieceName)
          }
        }
        else {
          if (!isWonder(pieceName) && (selectedCity.buildings.contains(externalInfo.pieceMap(pieceName)) || pieceName == "warrior")) {
            selectedCity.productionQueue = selectedCity.productionQueue :+ externalInfo.pieceMap(pieceName)
          }
        }
      case ClearQueue(selectedCityId, isScience, clearEntireQueue) =>
        val selectedCity = pieceById(selectedCityId)
        if (isScience) {
          if (selectedCity.scienceQueue.length > 0) {
            selectedCity.scienceQueue = List(selectedCity.scienceQueue.head)
            if (clearEntireQueue) {
              selectedCity.carriedScience = selectedCity.carriedScience * Constants.SCIENCE_DECAY_RATE
              selectedCity.scienceQueue = List()
            }
          }
        }
        else {
          if (selectedCity.productionQueue.length > 0) {
            selectedCity.productionQueue = List(selectedCity.productionQueue.head)
            if (clearEntireQueue) {
              // The below is equivalent to just applying Constants.PRODUCTION_DECAY_RATE to the carriedProduction,
              // unless you have an unbuild unit sitting around, in which case it's applied only to the cost of the
              // unbuilt unit
              val newCarriedProduction = selectedCity.carriedProduction - java.lang.Math.min(selectedCity.carriedProduction, selectedCity.productionQueue.head.productionCost.asInstanceOf[Double]) * (1 - Constants.PRODUCTION_DECAY_RATE)
              selectedCity.carriedProduction = newCarriedProduction
                - java.lang.Math.min(selectedCity.carriedProduction, selectedCity.productionQueue.head.productionCost.asInstanceOf[Double]) * (1 - Constants.PRODUCTION_DECAY_RATE)
              selectedCity.productionQueue = List()
            }
          }
        }
      case SetTarget(selectedPieceId, target) =>
        val selectedPiece = pieceById(selectedPieceId)
        if (selectedPiece.baseStats.name == "city" || selectedPiece.baseStats.leadership) {
          selectedPiece.target = target
          if (selectedPiece.baseStats.leadership) {
            setTargetsOfFollowers(selectedPiece)
          }
        } 
      case SetFocus(selectedCityId, focus) =>
        val selectedCity = pieceById(selectedCityId)
        selectedCity.focus = focus
      case PieceSuicide(selectedPieceId) =>
        val selectedPiece = pieceById(selectedPieceId)
        if (!isCityLike(selectedPiece.baseStats)) {
          killPiece(selectedPiece, externalInfo, true)
        }
      case FoundCity(side, loc) =>
        foundCity(side, loc, externalInfo)
    }
  }

//   def spawnPieceInternal(spawnSide: Side, spawnStats: PieceStats, spawnLoc: Loc, target: Loc, food: Double, 
    // production: Double, science: Double, cityFoundingSlack: Int = 0): Option[Piece] = {

  private def foundCity(side: Side, loc: Loc, externalInfo: ExternalInfo): Unit = {
    val (distanceToNearestFriendlyCity, _) = distanceToNearestCity(side, loc);
    val maximumPossibleDistance = maximumFoundCityDistanceFromFriendlyCity(side);
    val cityFoundingSlack = maximumPossibleDistance - distanceToNearestFriendlyCity
    if (canFoundCityAtLoc(side, loc)) {
      val _ = spawnPieceInternal(side, externalInfo.pieceMap("city"), loc, loc, 0, 0, Constants.SCIENCE_FOR_NEW_CITY * citiesFounded.get(side).getOrElse(0), 
        cityFoundingSlack=cityFoundingSlack)
    }
  }

  private def setTargetsOfFollowers(leader: Piece): Unit = {
    topology.forEachAdjRange2(leader.loc) { loc =>
      if (locIsValid(loc)) {
        val piecesOnLoc = pieces(loc)
        if (piecesOnLoc.length > 0) {
          val pieceOnLoc = piecesOnLoc.head
          if (!isDead(pieceOnLoc)) {
            if (pieceOnLoc.side == leader.side && !pieceOnLoc.baseStats.leadership) {
              pieceOnLoc.target = leader.target
            }
          }
        }
      }
    }  
  }

  private def applyEffect(effect: TargetEffect, piece: Piece, externalInfo: ExternalInfo, instigator: Option[Piece] = None): Unit = {
    effect match {
      case Damage(n) =>
        piece.damage += n
        killIfEnoughDamage(piece,externalInfo,instigator=instigator)
      case Unsummon =>
        unsummonPiece(piece)
      case Kill =>
        killPiece(piece,externalInfo)
      case Enchant(modWithDuration) =>
        piece.modsWithDuration = piece.modsWithDuration :+ modWithDuration
        killIfEnoughDamage(piece,externalInfo)
      case TransformInto(name) =>
        val stats = externalInfo.pieceMap(name)
        removeFromBoard(piece)
        spawnPieceInternal(piece.side,stats,piece.loc) match {
          case Some(_: Piece) => ()
          case None => ()
            //Piece was unable to legally belong on that square, so treat it as if killed
            updateAfterPieceKill(piece.side,stats,piece.loc,externalInfo)
        }
    }
  }

  //Kill a piece if it has enough accumulated damage
  private def killIfEnoughDamage(piece: Piece, externalInfo: ExternalInfo, instigator: Option[Piece] = None): Unit = {
    val stats = piece.curStats(this)
    stats.defense match {
      case None => ()
      case Some(defense) =>
        if(piece.damage >= defense) {
          killPiece(piece,externalInfo,instigator=instigator)
        }
    }
  }

  //Perform the rebase and death spawn updates happening after a piece kill
  private def updateAfterPieceKill(pieceSide: Side, pieceStats: PieceStats, loc: Loc, externalInfo: ExternalInfo): Unit = {
    //Death spawn
    pieceStats.deathSpawn.foreach { spawnName =>
      val spawnStats = externalInfo.pieceMap(spawnName)
      spawnPieceInternal(pieceSide,spawnStats,loc) match {
        case Some(_: Piece) => ()
        case None =>
          //Piece was unable to legally belong on that square, so treat it as if killed
          updateAfterPieceKill(pieceSide,spawnStats,loc,externalInfo)
      }
    }

    //Check for necromancers win condition - win when one is killed
    //(does not check side, simply relies on there not being a way to kill own necro on your turn)
    if(pieceStats.isNecromancer) {
      hasWon = true
    }
  }

  private def getTotalResourcesWithinRange2BySide(centralLoc: Loc): Map[Side, Double] = {
    var result: Map[Side, Double] = Map()
    tiles.topology.forEachAdjRange2(centralLoc) { loc =>
      if (locIsValid(loc)) {
        val piecesOnLoc = pieces(loc)
        if (piecesOnLoc.length > 0) {
          val piece = piecesOnLoc.head
          if (!isDead(piece)) {
            val side = piece.side
            val currentSideResult = result.get(side).getOrElse(0.0)
            result += (side -> (currentSideResult + piece.food + piece.production))
          }
        }
      }
    }
    return result
  }

  private def distributeRewardsBySide(rewardsBySide: Map[Side, Int]): Unit = {
    Side.foreach { side =>
      val rewardToDistribute = rewardsBySide.get(side).getOrElse(0)
      var extraReward = 0
      if (wonderBuiltBySide("statue of zeus", side) && rewardToDistribute > 0) {
        extraReward = 1
      }
      turnsTillNextCityPermanentModifier += (side -> (-1 * (rewardToDistribute + extraReward)))
    }
  }

  private def distributeCampRewards(loc: Loc, instigator: Option[Piece]): Unit = {
    val resourcesBySide = getTotalResourcesWithinRange2BySide(loc)
    var rewardsBySide: Map[Side, Int] = Map()
    Side.foreach { side => 
      val resources = resourcesBySide.get(side).getOrElse(0.0)
      if (resources > 0.0) {
        rewardsBySide = rewardsBySide + (side -> 1)
      }
    }
    instigator match {
      case None =>
      case Some(piece) =>
        rewardsBySide = rewardsBySide + (piece.side -> 1)
    }
    distributeRewardsBySide(rewardsBySide)
  }

  private def distributeLairRewards(loc: Loc, instigator: Option[Piece]): Unit = {
    val resourcesBySide = getTotalResourcesWithinRange2BySide(loc)
    var rewardsBySide: Map[Side, Int] = Map()
    Side.foreach { side => 
      val resources = resourcesBySide.get(side).getOrElse(0.0)
      if (resources > 20.0) {
        rewardsBySide = rewardsBySide + (side -> 3)
      } else if (resources > 10.0) {
        rewardsBySide = rewardsBySide + (side -> 2)
      } else if (resources > 0.0) {
        rewardsBySide = rewardsBySide + (side -> 1)
      }
    }
    instigator match {
      case None =>
      case Some(piece) =>
        rewardsBySide = rewardsBySide + (piece.side -> 3)
    }
    distributeRewardsBySide(rewardsBySide)
  }  

  private def distributeCityRewards(lostCitySide: Side, loc: Loc, instigator: Option[Piece]): Unit = {
    val resourcesBySide = getTotalResourcesWithinRange2BySide(loc)
    var rewardsBySide: Map[Side, Int] = Map()
    Side.foreach { side => 
      if (side != lostCitySide) {
        val resources = resourcesBySide.get(side).getOrElse(0.0)
        if (resources > 20.0) {
          rewardsBySide = rewardsBySide + (side -> 3)
        } else if (resources > 10.0) {
          rewardsBySide = rewardsBySide + (side -> 2)
        } else if (resources > 0.0) {
          rewardsBySide = rewardsBySide + (side -> 1)
        }
      }
    }
    instigator match {
      case None =>
      case Some(piece) =>
        rewardsBySide = rewardsBySide + (piece.side -> 3)
    }
    rewardsBySide = rewardsBySide + (lostCitySide -> 5)
    distributeRewardsBySide(rewardsBySide)
  }

  //Kill a piece, for any reason
  private def killPiece(piece: Piece, externalInfo: ExternalInfo, suicide: Boolean = false, instigator: Option[Piece] = None): Unit = {
    if(piece.curStats(this).isSoulbound) {
      unsummonPiece(piece)
    } else {
      val currentLoc = piece.loc
      val lostCitySide = piece.side
      removeFromBoard(piece)
      killedThisTurn = killedThisTurn :+ ((piece.spec, piece.baseStats.name, piece.side, piece.loc))
      updateAfterPieceKill(piece.side,piece.curStats(this),piece.loc,externalInfo)

      // For some reason it's 2x more stuff than there should be without this, too lazy to figure
      // out why
      var multiplier: Double = 1.0
      if (suicide && piece.baseStats.name != "mule") {
        multiplier *= Constants.SUICIDE_TAX
      }

      val tile = tiles(piece.loc)
      tile.food = tile.food + (piece.food + piece.carriedFood) * multiplier
      tile.production = tile.production + (piece.production + piece.carriedProduction) * multiplier
      tile.science = tile.science + (piece.science + piece.carriedScience) * multiplier
      piece.food = 0.0
      piece.production = 0.0
      piece.science = 0.0
      piece.carriedFood = 0.0
      piece.carriedProduction = 0.0
      piece.carriedScience = 0.0
      makeDead(piece)

      if (piece.baseStats.name == "camp") {
        distributeCampRewards(currentLoc, instigator)
      }
      if (piece.baseStats.name == "lair") {
        distributeLairRewards(currentLoc, instigator)
      }
      if (piece.baseStats.name == "city") {
        distributeCityRewards(lostCitySide, currentLoc, instigator)
        piece.buildings.foreach { building =>
          if (isWonder(building.name)) {
            markWonderBuiltBySide(building.name, piece.side, false);
          }
          if (building.name == "salvager") {
            val salvagerBuildingsBuiltBySide = salvagerBuildingsBuilt.get(piece.side).getOrElse(0)
            salvagerBuildingsBuilt += (piece.side -> (salvagerBuildingsBuiltBySide - 1))
          }
        }
      }
    }
  }

  private def unsummonPiece(piece: Piece): Unit = {
    removeFromBoard(piece)
    unsummonedThisTurn = unsummonedThisTurn :+ ((piece.spec, piece.baseStats.name, piece.side, piece.loc))
    addReinforcementInternal(piece.side,piece.baseStats.name)
  }
  private def removeFromBoard(piece: Piece): Unit = {
    pieces(piece.loc) = pieces(piece.loc).filterNot { p => p.id == piece.id }
    cities = cities.filterNot { p => p.id == piece.id }
    pieceById = pieceById - piece.id
  }

  private def addReinforcementInternal(side: Side, pieceName: PieceName): Unit = {
    reinforcements(side) = reinforcements(side) + (pieceName -> (reinforcements(side).getOrElse(pieceName,0) + 1))
  }

  //Does check for legality of spawn, returning the piece on success
  def spawnPieceInternal(spawnSide: Side, spawnStats: PieceStats, spawnLoc: Loc): Option[Piece] = {
    if(!spawnIsLegal(spawnSide, spawnStats, spawnLoc))
      None
    else {
      val nthAtLoc = numPiecesSpawnedThisTurnAt.get(spawnLoc).getOrElse(0)
      val piece = Piece.createInternal(spawnSide, spawnStats, nextPieceId, spawnLoc, nthAtLoc)
      pieces(spawnLoc) = pieces(spawnLoc) :+ piece
      pieceById += (piece.id -> piece)
      nextPieceId += 1
      numPiecesSpawnedThisTurnAt += (spawnLoc -> (nthAtLoc+1))     
      if (spawnStats.name == "city") {
        cities = cities :+ piece
        val citiesFoundedBySide = citiesFounded.get(spawnSide).getOrElse(0)
        citiesFounded += (spawnSide -> (citiesFoundedBySide + 1))        
      }
      if (spawnStats.name == "camp") {
        cities = cities :+ piece
        val rand = new Random(seed)
        seed = rand.nextInt(100000)
        val nextInt = rand.nextInt(4)
        if (nextInt == 0) {
          piece.focus = "warrior"
        }
        if (nextInt == 1) {
          piece.focus = "archer"
        }
        if (nextInt == 2) {
          piece.focus = "skirmisher"
        }
        if (nextInt == 3) {
          piece.focus = "legionary"
        }
      }      
      if (spawnStats.name == "lair") {
        cities = cities :+ piece
      }      
      Some(piece)
    }
  }

  //Does check for legality of spawn, returning the piece on success
  def spawnPieceInternal(spawnSide: Side, spawnStats: PieceStats, spawnLoc: Loc, target: Loc, food: Double, 
    production: Double, science: Double, cityFoundingSlack: Int = 0, extraHealth: Int = 0): Option[Piece] = {

    if(!spawnIsLegal(spawnSide, spawnStats, spawnLoc))
      None
    else {
      val nthAtLoc = numPiecesSpawnedThisTurnAt.get(spawnLoc).getOrElse(0)
      val piece = Piece.createInternal(spawnSide, spawnStats, nextPieceId, spawnLoc, nthAtLoc, target, food, production,
        science)
      pieces(spawnLoc) = pieces(spawnLoc) :+ piece
      pieceById += (piece.id -> piece)
      nextPieceId += 1
      numPiecesSpawnedThisTurnAt += (spawnLoc -> (nthAtLoc+1))
      piece.damage -= extraHealth
      if (spawnStats.name == "city") {
        cities = cities :+ piece
        val citiesFoundedBySide = citiesFounded.get(spawnSide).getOrElse(0)
        citiesFounded += (spawnSide -> (citiesFoundedBySide + 1))
        turnsTillNextCityTemporaryModifier += (spawnSide -> (-1 * cityFoundingSlack))
        setCityName(piece, randomizedCityNames(numCitiesFounded))
        piece.food += 2*Constants.POPULATION_BASE_COST + 1
        piece.population += 2
        piece.damage -= 2
      }   
      if (spawnStats.name == "camp") {
        cities = cities :+ piece
        val rand = new Random(seed)
        seed = rand.nextInt(100000)
        val nextInt = rand.nextInt(4)
        if (nextInt == 0) {
          piece.focus = "warrior"
        }
        if (nextInt == 1) {
          piece.focus = "archer"
        }
        if (nextInt == 2) {
          piece.focus = "skirmisher"
        }
        if (nextInt == 3) {
          piece.focus = "legionary"
        }
      } 
      if (spawnStats.name == "lair") {
        cities = cities :+ piece
      }      
      Some(piece)
    }
  }

  private def locIsOccupied(loc: Loc): Boolean = {
    if (!locIsValid(loc)) {
      return true
    }
    if (pieces(loc).length > 0) {
      return true
    }
    return false
  }

  private def locIsOccupiedByCityOrCivilian(loc: Loc): Boolean = {
    if (!locIsValid(loc)) {
      return true
    }
    if (pieces(loc).length > 0 && (isCityLike(pieces(loc).head.baseStats)
                                   || getAttackOfPiece(pieces(loc).head) == 0)) {
      return true
    }
    return false
  }

  private def smartDistanceTiebreaker(loc1: Loc, loc2: Loc): Int = {
    val xDifference = loc1.x - loc2.x
    val yDifference = loc1.y - loc2.y

    val maxVal = tiles.topology.distance(loc1, loc2)
    return java.lang.Math.max(java.lang.Math.min(
      java.lang.Math.abs(xDifference + maxVal),
      java.lang.Math.min(java.lang.Math.abs(xDifference),
      java.lang.Math.abs(xDifference - maxVal)),
    ),java.lang.Math.min(
      java.lang.Math.abs(yDifference + maxVal),
      java.lang.Math.min(java.lang.Math.abs(yDifference),
      java.lang.Math.abs(yDifference - maxVal)),
    ))
  }

  def smartDistance(loc1: Loc, loc2: Loc): Double = {
    return tiles.topology.distance(loc1, loc2) - 0.01 * smartDistanceTiebreaker(loc1, loc2)
  }

  private def buildUnit(city: Piece, unit: PieceStats, foodCost: Double, productionCost: Double): Boolean = {
    var bestLoc: Option[Loc] = None
    var bestDistance: Double = 100.0
    var currentDistance: Double = 0.0
    var extraHealth = 0
    if (unit.name == "salvager") {
      extraHealth = salvagerBuildingsBuilt.get(city.side).getOrElse(0)
    }
    if (wonderBuiltBySide("junkotron", city.side)) {
      extraHealth = extraHealth * 2 + 5  // 5 should be equal to salvager base health
    }
    tiles.topology.forEachAdjRange2(city.loc) { loc =>
      currentDistance = tiles.topology.distance(city.loc, loc) + 0.01 * smartDistance(city.target, loc)
      if (!locIsOccupied(loc) && currentDistance < bestDistance) {
        bestDistance = currentDistance
        bestLoc = Some(loc)
      }
    }
    bestLoc match {
      case None => return false
      case Some(locToSpawnOn) => 
        spawnPieceInitial(city.side,unit,locToSpawnOn,city.target, foodCost, productionCost, 0.0, 
          extraHealth=extraHealth)
        return true
    }
  }

  private def buildUnits(city: Piece): Unit = {
    if (city.productionQueue.size > 0) {
      val productionQueue = city.productionQueue;
      val nextProductionUnit = productionQueue.head
      val productionCost = nextProductionUnit.productionCost.asInstanceOf[Double];
      if ((productionCost <= city.carriedProduction) && (city.population > 1)) {
        var foodCost = getCostOfLastPopulation(city)
        if (nextProductionUnit.name == "warrior") {
          foodCost = 0
        }
        if (buildUnit(city, nextProductionUnit, foodCost, productionCost)) {
          city.productionQueue = productionQueue.slice(1, productionQueue.size);
          city.carriedProduction = city.carriedProduction - productionCost;
          if (nextProductionUnit.name != "warrior") {
            city.population = city.population - 1;
            city.food = city.food - foodCost;
            val stats = city.curStats(this)
            stats.defense match {
              case None => ()
              case Some(defense) =>
                if (city.population < Constants.CITY_POPULATION_KINK_1) {
                  city.damage = Math.min(defense - 1, city.damage + 2)
                }
                else if (city.population >= Constants.CITY_POPULATION_KINK_1 && city.population >= Constants.CITY_POPULATION_KINK_2) {
                  city.damage = Math.min(defense - 1, city.damage + 1)
                }       
            }       
          }
          buildUnits(city);
        } else {
          // Decay the city's excess production if the build failed
          city.carriedProduction = city.carriedProduction - (city.carriedProduction - productionCost) * (1 - Constants.PRODUCTION_DECAY_RATE)
        }
      }
    }
  }

  private def buildBuildings(city: Piece): Unit = {
    if (city.scienceQueue.size > 0) {
      val scienceQueue = city.scienceQueue;
      val nextScienceUnit = scienceQueue.head;
      val scienceCost = nextScienceUnit.scienceCost;
      if (scienceCost <= city.carriedScience) {
        city.buildings = city.buildings ::: List(nextScienceUnit);
        city.scienceQueue = scienceQueue.slice(1, scienceQueue.size);
        city.carriedScience = city.carriedScience - scienceCost;
        city.science = city.science + scienceCost;
        if (nextScienceUnit.name == "salvager") {
          val citySide = city.side
          val salvagerBuildingsBuiltBySide = salvagerBuildingsBuilt.get(citySide).getOrElse(0)
          salvagerBuildingsBuilt += (citySide -> (salvagerBuildingsBuiltBySide + 1))
          pieceById.keySet.foreach(pieceId => {
            val piece = pieceById(pieceId)
            if (piece.baseStats.name == "salvager" && piece.side == city.side) {
              if (wonderBuiltBySide("junkotron", city.side)) {
                piece.damage -= 2
              } else {
                piece.damage -= 1
              }
            }
          })
        }
        if (isWonder(nextScienceUnit.name)) {
          val name = nextScienceUnit.name
          markWonderBuiltBySide(name, city.side)
          clearWonderFromQueues(name)
        }
        if (nextScienceUnit.name == "junkotron") {
          val citySide = city.side
          val salvagerBuildingsBuiltBySide = salvagerBuildingsBuilt.get(citySide).getOrElse(0)
          salvagerBuildingsBuilt += (citySide -> (salvagerBuildingsBuiltBySide + 1))
          pieceById.keySet.foreach(pieceId => {
            val piece = pieceById(pieceId)
            if (piece.baseStats.name == "salvager" && piece.side == city.side) {
              piece.damage -= salvagerBuildingsBuiltBySide 
              piece.damage -= 5
            }
          })          
        }
        buildBuildings(city);
      }
    }
  }

  private def increasePoisonOfPiece(piece: Piece, extraPoison: Int): Unit = {
    val (poison, other, cityName, dead) = piece.modifiers
    piece.modifiers = (poison + extraPoison, other, cityName, dead)
  }

  private def setCityName(piece: Piece, cityName: String): Unit = {
    val (poison, other, _, dead) = piece.modifiers
    piece.modifiers = (poison, other, cityName, dead)
  }

  def getCityName(piece: Piece): String = {
    return piece.modifiers._3
  }

  private def makeDead(piece: Piece): Unit = {
    val (poison, other, cityName, _) = piece.modifiers
    piece.modifiers = (poison, other, cityName, true)
  }

  def isDead(piece: Piece): Boolean = {
    return piece.modifiers._4
  }

  private def totalResourcesOnLoc(tile: Tile): Double = {
    return tile.food + tile.production + tile.science
  }

  private def capacity(salvager: Piece): Int = {
    var multiplier = 1
    if (wonderBuiltBySide("junkotron", salvager.side)) {
      multiplier = 2
    }
    return (5 + salvagerBuildingsBuilt.get(salvager.side).getOrElse(0)) * multiplier
  }

  private def totalResourcesCarried(salvager: Piece): Double = {
    return salvager.carriedFood + salvager.carriedProduction + salvager.carriedScience
  }

  private def remainingResourceSpace(salvager: Piece): Double = {
    return capacity(salvager) - totalResourcesCarried(salvager)
  }

  private def getBaseAttackOfPiece(piece: Piece): Int = {
    piece.baseStats.attackEffect match {
      case None =>
        return 0
      case Some(Damage(n)) =>
        return n
      case Some(Unsummon) =>
        return 0
      case Some(Kill) =>
        return 0
      case Some(Enchant(_)) =>
        return 0
      case Some(TransformInto(_)) =>
        return 0
    }
  }

  private def locIsValid(loc: Loc): Boolean = {
    if (loc.x >= 0 && loc.x < xSize && loc.y >= 0 && loc.y < ySize) {
      tiles(loc).terrain match {
        case Water(_) => 
          return false
        case Wall | Ground | Water(_) | Graveyard | SorceryNode | Teleporter |
                 Earthquake(_) | Firestorm(_) | Whirlwind(_) | Mist | Spawner(_) =>
            return true
          }
        }
    return false
  }

  // (isGeneral, blank)
  private def getModifiersInRange(piece: Piece): (Boolean, Boolean, Boolean) = {
    var isGeneral = false
    var isBanshee = false
    var isKhan = false
    topology.forEachAdjRange2(piece.loc) { loc =>
      if (locIsValid(loc)) {
        val piecesOnLoc = pieces(loc)
        if (piecesOnLoc.length > 0) {
          val pieceOnLoc = piecesOnLoc.head
          if (!isDead(pieceOnLoc)) {
            if (pieceOnLoc.baseStats.name == "general" && pieceOnLoc.side == piece.side) {
              isGeneral = true
            }
            if (pieceOnLoc.baseStats.name == "khan" && pieceOnLoc.side == piece.side) {
              isKhan = true
            }          
            if (pieceOnLoc.baseStats.name == "banshee" && pieceOnLoc.side != piece.side) {
              isBanshee = true
            }
          }
        }
      }
    }
    return (isGeneral, isBanshee, isKhan)
  }

  def getAttackOfPiece(piece: Piece, mainAttack: Boolean = true): Int = {
    if (!mainAttack) {
      return piece.baseStats.splash
    }
    val (isGeneral, isBanshee, _) = getModifiersInRange(piece)
    var attack = getBaseAttackOfPiece(piece)
    if (attack == 0) {
      return 0
    }
    if (isGeneral) {
      attack += 1
    }
    if (isBanshee) {
      attack -= 2
    }
    if (wonderBuiltBySide("dream twister", piece.side)) {
      attack += 1
    }
    if (piece.baseStats.name == "city") {
      if (piece.population >= 5) {
        attack += 1
      }
      if (piece.population >= 13) {
        attack += 1
      }
      if (piece.population >= 23) {
        attack += 1
      }
    }
    return attack
  }

  private def getRemainingHealthOfPiece(piece: Piece): Int = {
    val stats = piece.curStats(this)
    stats.defense match {
      case None => 
        return 0
      case Some(defense) =>
        return defense - piece.damage
      }
  }

  def isCityLike(pieceStats: PieceStats): Boolean = {
    return pieceStats.name == "city" || pieceStats.name == "camp" || pieceStats.name == "lair"
  }

  def immuneToPoison(pieceStats: PieceStats): Boolean = {
    return isCityLike(pieceStats) || pieceStats.name == "giant frog" || pieceStats.name == "cobra car"
  }

  private def juiciness(piece: Piece): Double = {
    return (piece.food + piece.production) / (getRemainingHealthOfPiece(piece))
  }

  private def locIsAdjacentToEnemyMeleeUnit(loc: Loc, side: Side): Boolean = {
    topology.forEachAdj(loc) { loc =>
      if (locIsValid(loc)) {
        val piecesOnLoc = pieces(loc)
        if (piecesOnLoc.length > 0) {
          val pieceOnLoc = piecesOnLoc.head
          if (!isDead(pieceOnLoc)) {
            if (pieceOnLoc.baseStats.attackRange == 1 && pieceOnLoc.side != side) {
              return true
            }
          }
        }
      }
    }
    return false
  }

  private def adjacentUnoccupiedHex(piece: Piece, notAdjacentToEnemyMeleeUnit: Boolean = false): Option[Loc] = {
    var bestLoc: Option[Loc] = None
    var bestScore: Double = -1000.0

    topology.forEachAdj(piece.loc) { loc =>
      if (!locIsOccupied(loc) && (!locIsAdjacentToEnemyMeleeUnit(loc, piece.side) || !notAdjacentToEnemyMeleeUnit)) {
        val score = -1 * smartDistance(loc, piece.target)
        if (score > bestScore) {
          bestScore = score
          bestLoc = Some(loc)
        }
      }
    }
    return bestLoc
  }  

  private def stealResources(piece: Piece, target: Piece, amount: Double) = {
    var remainingAmountToSteal = amount
    val tile = tiles(piece.loc)
    if (remainingAmountToSteal > 0.01) {
      val amountToSteal = Math.min(remainingAmountToSteal, target.carriedScience)
      tile.science += amountToSteal
      target.carriedScience -= amountToSteal
      remainingAmountToSteal -= amountToSteal
    }
    if (remainingAmountToSteal > 0.01) {
      val amountToSteal = Math.min(remainingAmountToSteal, target.carriedProduction)
      tile.production += amountToSteal
      target.carriedProduction -= amountToSteal
      remainingAmountToSteal -= amountToSteal
    }
    if (remainingAmountToSteal > 0.01) {
      val amountToSteal = Math.min(remainingAmountToSteal, target.carriedFood)
      tile.food += amountToSteal
      target.carriedFood -= amountToSteal
    }        
  }

  private def getScoreForDamageToTarget(damage: Int, target: Piece): Double = {
    var totalScore: Double = 0.0
    val targetRemainingHealth = getRemainingHealthOfPiece(target)
    val damageWouldBeDealt = java.lang.Math.min(damage, targetRemainingHealth)

    // Increase the score by the amount of the damage you would deal
    totalScore = totalScore + damageWouldBeDealt

    // Increase the score by 5 if you would kill the target
    if (damage >= targetRemainingHealth) {
      totalScore = totalScore + 5.0
    }

    // If all else is equal, attack the juicier target
    totalScore = totalScore + 0.001 * damage * juiciness(target)

    return totalScore
  }

  private def isRanged(piece: Piece): Boolean = {
    return piece.baseStats.attackRange > 1
  }

  // Differs from getDamageDealtToTarget by taking into account max health
  private def getDamageWouldBeDealt(piece: Piece, target: Piece): Int = {
    val damage = getDamageDealtToTarget(piece, target)
    val targetRemainingHealth = getRemainingHealthOfPiece(target)
    return java.lang.Math.min(damage, targetRemainingHealth)
  }

  private def getRetaliateAttackEffect(piece: Piece, target: Piece): (TargetEffect, Boolean) = {
    if (target.baseStats.retaliate && !isRanged(piece)) {
      return (Damage(getDamageDealtToTarget(target, piece)), true)
    }
    return (Damage(0), false)
  }

  private def getAttackEffect(piece: Piece, target: Piece): TargetEffect = {
    return Damage(getDamageDealtToTarget(piece, target))
  }

  private def getDamageDealtToTarget(piece: Piece, target: Piece, mainAttack: Boolean = true): Int = {
    var damage = getAttackOfPiece(piece, mainAttack=mainAttack)
    if (isRanged(piece) && target.baseStats.nimble) {
      damage = damage / 2
    }
    if ((piece.baseStats.name == "trebuchet" || piece.baseStats.name == "cannon") && isCityLike(target.baseStats)) {
      damage = damage * 3
    }
    if (piece.baseStats.name == "telekinetic" && target.baseStats.name == "salvager") {
      damage = damage * 3
    }
    damage = Math.max(damage - target.baseStats.robust, 0)
    return damage
  }

  private def getScoreForAttack(piece: Piece, target: Piece, mainAttack: Boolean = true, ignoreTaunt: Boolean = false): Double = {
    var totalScore: Double = 0.0

    val damageDealtToTarget = getDamageDealtToTarget(piece, target, mainAttack=mainAttack)
    if (damageDealtToTarget <= 0 && piece.baseStats.poisonous == 0 && !target.baseStats.taunt && piece.baseStats.splash == 0) {
      // Harshly penalize attacks that do nothing so that they don't happen
      totalScore -= 10000.0
    }
    if (isCityLike(piece.baseStats) && isCityLike(target.baseStats)) {
      // Cities should never want to attack other cities
      totalScore -= 10000.0
    }

    // Increase the score by the amount of the damage you would deal
    totalScore = totalScore + getScoreForDamageToTarget(damageDealtToTarget, target)

    // Reward stealing resources
    if (piece.baseStats.name == "telekinetic") {
      totalScore = totalScore + Math.min(totalResourcesCarried(target), getDamageWouldBeDealt(piece, target).asInstanceOf[Double])
    }

    // Decrease score by each stack of poison on the target
    val poisonOnTarget = target.modifiers._1
    totalScore = totalScore - 2.0 * poisonOnTarget.asInstanceOf[Double]

    // If the attacker is poisonous, prioritize larger targets, further adjusting downwards if the target
    // is already poisoned
    if (piece.baseStats.poisonous > 0) {
      totalScore = totalScore + getRemainingHealthOfPiece(target) - 2.0 * poisonOnTarget.asInstanceOf[Double]
      if (immuneToPoison(target.baseStats)) {
        totalScore -= 4.0 * piece.baseStats.poisonous
      }
    }

    // Prioritize attacking taunt units
    if (target.baseStats.taunt && !ignoreTaunt) {
      totalScore += 100.0
    }

    return totalScore
  }

  private def getScoreForAttackAccountingForSplash(piece: Piece, target: Piece): Double = {
    var totalScore = getScoreForAttack(piece, target);
    if (piece.baseStats.splash == 0) {
      return totalScore
    }
    tiles.topology.forEachAdj(target.loc) { loc =>
      if (locIsValid(loc)) {
        val piecesOnLoc = pieces(loc)
        if (piecesOnLoc.length > 0) {
          val splashTarget = piecesOnLoc.head
          if (!isDead(splashTarget)) { 
            if (splashTarget.side != piece.side) {
              totalScore = totalScore + getScoreForAttack(piece, target, mainAttack=false)
            }
            if (splashTarget.side == piece.side) {
              totalScore = totalScore - getScoreForAttack(piece, target, mainAttack=false, ignoreTaunt=true)
            }          
          }
        }
      }
    }
    return totalScore
  }

  private def ignoresMelee(piece: Piece): Boolean = {
    return piece.baseStats.name == "hussar" || piece.baseStats.name == "giant frog"
  }

  private def getBestTargetForAttack(piece: Piece): Option[Piece] = {
    val range = piece.baseStats.attackRange;
    var offsets = List(Vec(0,0))
    if (range == 1) {
      offsets = tiles.topology.adjOffsets;
    } else if (range == 2) {
      offsets = tiles.topology.adjOffsetsRange2;
    } else if (range == 3) {
      offsets = tiles.topology.adjOffsetsRange3;
    }

    var bestTarget: Option[Piece] = None
    var bestScore: Double = -1000.0

    offsets.foreach {vec => 
      val loc = piece.loc + vec
      if (locIsValid(loc)) {
        if (getAttackOfPiece(piece) > 0) {
          val piecesOnLoc = pieces(loc)
          if (piecesOnLoc.length > 0) {
            val target = piecesOnLoc.head
            if (!isDead(target)) {
              if (target.side != piece.side && (!ignoresMelee(piece) || target.baseStats.attackRange > 1)) {
                val score = getScoreForAttackAccountingForSplash(piece, target)
                if (score > bestScore) {
                  bestScore = score
                  bestTarget = Some(target)
                }
              }
            }
          }
        }
      }
    }

    return bestTarget
  }

  def canFoundCityAtLoc(side: Side, loc: Loc): Boolean = {
    val (distanceToNearestFriendlyCity, distanceToNearestAnyCity) = distanceToNearestCity(side, loc);
    return (distanceToNearestFriendlyCity <= maximumFoundCityDistanceFromFriendlyCity(side)
            && distanceToNearestAnyCity >= minimumFoundCityDistanceFromCity(side)
            && tiles(loc).hasNearbyFriendly.get(side).getOrElse(false)
            && !tiles(loc).hasNearbyEnemy.get(side).getOrElse(false))
  }

  def nearestFriendlyCity(side: Side, loc: Loc): Option[Piece] = {
    var distanceToNearestFriendlyCity = 100.0
    var nearestCity: Option[Piece] = None
    cities.foreach(city => {
      if (city.side == side) {
        val distance = smartDistance(city.loc, loc)
        distanceToNearestFriendlyCity = java.lang.Math.min(distanceToNearestFriendlyCity, distance)
        nearestCity = Some(city)
      }
    })
    return nearestCity
  }

  // Returns tuple of (distance to nearest friendly city, distance to nearest city) 
  def distanceToNearestCity(side: Side, loc: Loc): (Int, Int) = {
    var distanceToNearestFriendlyCity = 100
    var distanceToNearestAnyCity = 100
    cities.foreach(city => {
      val distance = tiles.topology.distance(city.loc, loc)
      distanceToNearestAnyCity = java.lang.Math.min(distanceToNearestAnyCity, distance)
      if (city.side == side || city.side == SB) {
        distanceToNearestFriendlyCity = java.lang.Math.min(distanceToNearestFriendlyCity, distance)
      }
    })
    return (distanceToNearestFriendlyCity, distanceToNearestAnyCity)
  }

  private def minimumFoundCityDistanceFromCity(side: Side): Int = {
    if (wonderBuiltBySide("cloning vats", side)) {
      return 1
    }
    return 3
  }

  private def maximumFoundCityDistanceFromFriendlyCity(side: Side): Int = {
    return cityPower(side) + 3
  }

  def cityPower(side: Side): Int = {
    return turnNumber - citiesFounded.get(side).getOrElse(0) * 10 - turnsTillNextCityTemporaryModifier.get(side).getOrElse(0) - turnsTillNextCityPermanentModifier.get(side).getOrElse(0)
  }

  private def getScoreForMoveTowards(piece: Piece, target: Piece): Double = {
    var totalScore: Double = 0.0

    // Decrease the score by how far the target is from our target hex
    totalScore = totalScore - smartDistance(target.loc, piece.target)

    // If all else is equal, chase the juicier target
    totalScore = totalScore + 0.001 * juiciness(target)    

    // Giant frogs need an adjacent hex to jump into
    if (piece.baseStats.name == "giant frog") {
      val adjHex = adjacentUnoccupiedHex(target)
      adjHex match {
        case None => 
          totalScore = totalScore - 10000.0
        case Some(_) => 
      }
    }

    return totalScore
  }

  private def getBestTargetForMoveTowards(piece: Piece): Option[Piece] = {
    val offsets = tiles.topology.adjOffsetsRange3;

    var bestTarget: Option[Piece] = None
    var bestScore: Double = -1000.0

    offsets.foreach {vec =>
      val loc = piece.loc + vec
      if (locIsValid(loc)) {
        val piecesOnLoc = pieces(loc)
        if (piecesOnLoc.length > 0) {
          val target = piecesOnLoc.head
          if (!isDead(target)) {
            if (target.side != piece.side && (!ignoresMelee(piece) || target.baseStats.attackRange > 1)) {
              val score = getScoreForMoveTowards(piece, target)
              if (score > bestScore) {
                bestScore = score
                bestTarget = Some(target)
              }
            }
          }
        }
      }
    }    

    return bestTarget
  }

  private def getBestTargetForSalvagerMoveTowards(piece: Piece): Loc = {
    val resourceSpace = remainingResourceSpace(piece)
    val offsets = tiles.topology.adjOffsetsRange3;    

    if (resourceSpace > 0.01) {
      var bestTarget: Loc = piece.target
      var bestScore: Double = 0.0

      offsets.foreach {vec =>
        val loc = piece.loc + vec
        if (locIsValid(loc)) {    
            val potentialResourcesToGet = java.lang.Math.min(totalResourcesOnLoc(tiles(loc)), remainingResourceSpace(piece))
            val score = potentialResourcesToGet / (tiles.topology.distance(loc, piece.target) + 1)
            if (score > bestScore) {
              bestScore = score
              bestTarget = loc
            }
          }
        }
      return bestTarget
    }
    else {
      val nearestCity = nearestFriendlyCity(piece.side, piece.loc)
      nearestCity match {
        case None => return piece.loc
        case Some(city) => return city.loc
      }
    }
  }

  private def moveTowards(piece: Piece, target: Loc): Boolean = {
    var bestLoc: Loc = piece.loc
    var bestDistance: Double = smartDistance(piece.loc, target)
    var currentDistance: Double = 0.0
    tiles.topology.forEachAdj(piece.loc) { loc =>
      currentDistance = smartDistance(loc, target)
      if (!locIsOccupied(loc) && currentDistance < bestDistance) {
        bestDistance = currentDistance
        bestLoc = loc
      }
    }
    if (bestLoc != piece.loc) {
      doMovePieceToLoc(piece, bestLoc)
      return true
    }
    return false
  }

  private def civilianMoveTowards(piece: Piece, target: Loc): Boolean = {
    var bestLoc: Loc = piece.loc
    var bestDistance: Double = smartDistance(piece.loc, target)
    var currentDistance: Double = 0.0
    tiles.topology.forEachAdj(piece.loc) { loc =>
      currentDistance = smartDistance(loc, target)
      if (!locIsOccupiedByCityOrCivilian(loc) && currentDistance < bestDistance) {
        bestDistance = currentDistance
        bestLoc = loc
      }
    }
    if (bestLoc != piece.loc) {
      if (locIsOccupied(bestLoc)) {
        val pieceAtBestLoc = pieces(bestLoc).head
        doMovePieceToLoc(pieceAtBestLoc, piece.loc)
        doMovePieceToLoc(piece, bestLoc)
      } 
      else {
        doMovePieceToLoc(piece, bestLoc)       
      }
      return true 
    }
    return false
  }

  private def salvagerPickUpResourcesFromTile(salvager: Piece, tile: Tile): Unit = {
    var remainingSpace = remainingResourceSpace(salvager)
    if (remainingSpace > 0.01) {
      val amountToPickUp = Math.min(remainingSpace, tile.science)
      tile.science -= amountToPickUp
      salvager.carriedScience += amountToPickUp
      remainingSpace -= amountToPickUp
    }
    if (remainingSpace > 0.01) {
      val amountToPickUp = Math.min(remainingSpace, tile.production)
      tile.production -= amountToPickUp
      salvager.carriedProduction += amountToPickUp
      remainingSpace -= amountToPickUp
    }
    if (remainingSpace > 0.01) {
      val amountToPickUp = Math.min(remainingSpace, tile.food)
      tile.food -= amountToPickUp
      salvager.carriedFood += amountToPickUp
    }    
  }

  private def attackMoveInner(piece: Piece, externalInfo: ExternalInfo, remainingMovement: Int): Unit = {
    if (remainingMovement > 1 && (piece.baseStats.name == "horse archer" || piece.baseStats.name == "cobra car")) {
      val locToFleeTo = adjacentUnoccupiedHex(piece, notAdjacentToEnemyMeleeUnit=true)
      locToFleeTo match { 
        case None =>
        case Some(loc) =>
          doMovePieceToLoc(piece, loc)
      }
      attackMoveInner(piece, externalInfo, remainingMovement - 1)
    }
    else if (remainingMovement > 0) {
      if (getAttackOfPiece(piece) > 0) {
        if (!tryAttacking(piece, externalInfo)) {
          val targetForMoveTowards = getBestTargetForMoveTowards(piece)
          var targetLoc = piece.loc
          var didJump = false
          targetForMoveTowards match {
            case Some(target) =>
              targetLoc = target.loc
              if (piece.baseStats.name == "giant frog") {
                val adjHex = adjacentUnoccupiedHex(target)
                // Should never be null, if there's a valid target
                adjHex match {
                  case None =>
                  case Some(loc) =>
                    doMovePieceToLoc(piece, loc)
                    attackMoveInner(piece, externalInfo, remainingMovement - 1)
                    didJump = true
                }
              }              
            case None =>
              targetLoc = piece.target
          }
          if (!didJump && moveTowards(piece, targetLoc)) {
            attackMoveInner(piece, externalInfo, remainingMovement - 1)
          }
        }
      }
      else if (piece.baseStats.name == "salvager") {
        val targetForMoveTowards = getBestTargetForSalvagerMoveTowards(piece)
        civilianMoveTowards(piece, targetForMoveTowards) 
        attackMoveInner(piece, externalInfo, remainingMovement - 1)
      }
      else {
        val targetForMoveTowards = piece.target
        civilianMoveTowards(piece, targetForMoveTowards) 
        attackMoveInner(piece, externalInfo, remainingMovement - 1)
      }
    }
    else if (piece.baseStats.charge) {
      val _ = tryAttacking(piece, externalInfo)
    }
    else if (piece.baseStats.name == "salvager") {
      if (remainingResourceSpace(piece) <= 0.01) {
        // Try to drop off
        val nearestCity = nearestFriendlyCity(piece.side, piece.loc)
          nearestCity match {
            case None =>
            case Some(fakeCity) => 
              val fakeCityId = fakeCity.id
              val city = pieceById(fakeCityId)
              if (tiles.topology.distance(city.loc, piece.loc) <= 1) {
                val carriedFood = piece.carriedFood
                city.carriedFood = city.carriedFood + carriedFood
                piece.carriedFood = 0.0
                val carriedProduction = piece.carriedProduction
                city.carriedProduction = city.carriedProduction + carriedProduction
                piece.carriedProduction = 0.0
                val carriedScience = piece.carriedScience
                city.carriedScience = city.carriedScience + carriedScience
                piece.carriedScience = 0.0
          }
        }
      }
      else {
        // Try to pick up
        var offsets = tiles.topology.adjOffsets ::: List(Vec(0,0));
        if (wonderBuiltBySide("junkotron", piece.side)) {
          offsets = tiles.topology.adjOffsetsRange2 ::: List(Vec(0,0));
        }
        var adjLocs = offsets.map({vec => piece.loc + vec})
        adjLocs = adjLocs.filter(loc => locIsValid(loc))
        adjLocs = adjLocs.sortWith(smartDistance(_, piece.loc) < smartDistance(_, piece.loc))
        val adjTiles = adjLocs.map(loc => tiles(loc))
        adjTiles.foreach(tile => salvagerPickUpResourcesFromTile(piece, tile))
      }
    }
  }

  private def tryAttacking(piece: Piece, externalInfo: ExternalInfo): Boolean = {
    val targetForAttack = getBestTargetForAttack(piece)
    targetForAttack match {
      case Some(target) =>
        val attackEffect = getAttackEffect(piece, target)
        val (retaliateAttackEffect, didRetaliate) = getRetaliateAttackEffect(piece, target)    
        val targetRemainingHealth = getRemainingHealthOfPiece(target)
        val damageWouldBeDealt = getDamageWouldBeDealt(piece, target)
        val killingBlow = targetRemainingHealth <= damageWouldBeDealt
        val targetLoc = target.loc
        applyEffect(attackEffect,target,externalInfo,instigator=Some(piece))
        applyEffect(retaliateAttackEffect,piece,externalInfo,instigator=Some(piece))
        if (piece.baseStats.poisonous > 0 && !immuneToPoison(target.baseStats)) {
          increasePoisonOfPiece(target, piece.baseStats.poisonous)
        }
        if (target.baseStats.poisonous > 0 && didRetaliate && !immuneToPoison(piece.baseStats)) {
          increasePoisonOfPiece(piece, target.baseStats.poisonous)
        }
        if (piece.baseStats.name == "telekinetic" && totalResourcesCarried(target) > 0.0) {
          stealResources(piece, target, damageWouldBeDealt.asInstanceOf[Double])
        }
        if (piece.baseStats.splash > 0) {
          tiles.topology.forEachAdj(target.loc) { loc =>
            if (locIsValid(loc)) {
              val piecesOnLoc = pieces(loc)
              if (piecesOnLoc.length > 0) {
                val splashTarget = piecesOnLoc.head
                if (!isDead(splashTarget)) {
                  val splashEffect = Damage(piece.baseStats.splash)
                  applyEffect(splashEffect, splashTarget, externalInfo)
                }
              }
            }            
          }
        }
        if (piece.baseStats.name == "godzilla" && killingBlow) {
          doMovePieceToLoc(piece, targetLoc)
        }
        return true
      case None => 
        return false
    }
  }

  private def attackMove(piece: Piece, externalInfo: ExternalInfo): Unit = {
    val moveRange = piece.baseStats.moveRange
    if (moveRange == 0) {
      val _ = tryAttacking(piece, externalInfo)
    } else {
      val (_, _, isKhan) = getModifiersInRange(piece)
      var extraMovement = 0
      if (isKhan) {
        extraMovement += 1
      }
      if (piece.baseStats.name == "salvager" && wonderBuiltBySide("junkotron", piece.side)) {
        extraMovement += 1
      }
      attackMoveInner(piece, externalInfo, piece.baseStats.moveRange + extraMovement)
    }
  }

  private def getCostOfNextPopulationForGivenCurrentPopulation(population: Int): Double = {
    var total: Int = Constants.POPULATION_BASE_COST
    total += Math.min(population, Constants.CITY_POPULATION_KINK_1)
    total += Math.max((population - Constants.CITY_POPULATION_KINK_1)/2, 0)
    return Math.min(total, Constants.MAX_POPULATION_COST).asInstanceOf[Double]
  }

  private def getCostOfLastPopulation(piece: Piece): Double = {
    getCostOfNextPopulationForGivenCurrentPopulation(piece.population - 1);
  }

  private def getCostOfNextPopulation(piece: Piece): Double = {
    getCostOfNextPopulationForGivenCurrentPopulation(piece.population);
  }

  private def growPopulation(piece: Piece): Unit = {
    val costOfNextPopulation = getCostOfNextPopulation(piece);
    if (piece.carriedFood >= costOfNextPopulation) {
      piece.carriedFood = piece.carriedFood - costOfNextPopulation;
      piece.food = piece.food + costOfNextPopulation;
      piece.population = piece.population + 1;
      if (piece.population <= Constants.CITY_POPULATION_KINK_1) {
        piece.damage -= 2;
      } else if (piece.population > Constants.CITY_POPULATION_KINK_1 && piece.population <= Constants.CITY_POPULATION_KINK_2) {
        piece.damage -= 1;
      }
    }
  }

  private def getRandomLoc(): Loc = {
    val rand = new Random(seed);
    seed = rand.nextInt(100000)
    val possibleLoc = Loc(rand.nextInt(xSize), rand.nextInt(ySize))
    if (locIsValid(possibleLoc)) {
      return possibleLoc
    }
    else {
      return getRandomLoc()
    }
  }

  private def setRandomTarget(piece: Piece): Unit = {
    piece.target = getRandomLoc()
  }

  private def harvestYields(piece: Piece, tile: Tile): Unit = {
    var foodMultiplier = 1;
    if (wonderBuiltBySide("fast food chains", piece.side)) {
      foodMultiplier = 2
    }
    piece.carriedFood = piece.carriedFood + foodMultiplier * tile.foodYield;
    piece.carriedProduction = piece.carriedProduction + tile.productionYield;
    piece.carriedScience = piece.carriedScience + tile.scienceYield;
  }

  private def getCityGetExtraMaxHealth(city: Piece): Int = {
    return Math.min(2 * Math.min(city.population, Constants.CITY_POPULATION_KINK_1) 
      + Math.max(city.population - Constants.CITY_POPULATION_KINK_1, 0), 34)
  }

  private def refreshPieceForStartOfTurn(piece: Piece, externalInfo: ExternalInfo): Unit = {
    piece.actState = Moving(0)
    piece.hasMoved = false

    if (piece.baseStats.name == "city") {
      piece.damage = (piece.damage - 1).max(-1 * getCityGetExtraMaxHealth(piece));
      // Collect yields
      tiles.topology.forEachAdj(piece.loc) {
        loc => {
          if (locIsValid(loc)) {
            harvestYields(piece, tiles(loc))
          }
        }
      }
      harvestYields(piece, tiles(piece.loc))
      var foodMultiplier = 1;
      if (wonderBuiltBySide("fast food chains", piece.side)) {
        foodMultiplier = 2
      }      
      if (piece.focus == "food") {
        piece.carriedFood = piece.carriedFood + foodMultiplier * piece.population;
      } else if (piece.focus == "production") {
        piece.carriedProduction = piece.carriedProduction + piece.population;
      } else {
        piece.carriedScience = piece.carriedScience + piece.population;
      }
      // Allocate production and science to their respective queues
      growPopulation(piece);
      buildUnits(piece);
      buildBuildings(piece);

      // Penalize unallocated production and science
      if (piece.scienceQueue.size == 0) {
        piece.carriedScience = piece.carriedScience * Constants.SCIENCE_DECAY_RATE
      }
      if (piece.productionQueue.size == 0) {
        piece.carriedProduction = piece.carriedProduction * Constants.PRODUCTION_DECAY_RATE
      }
    }
    else if (piece.baseStats.name == "camp") {
      harvestYields(piece, tiles(piece.loc))
      if (piece.focus == "warrior" && turnNumber % 4 == 2) {
        setRandomTarget(piece)
        val _ = buildUnit(piece, externalInfo.pieceMap("warrior"), 2.0, 2.0)
      }
      if (piece.focus == "archer" && turnNumber % 6 == 3) {
        setRandomTarget(piece)
        val _ = buildUnit(piece, externalInfo.pieceMap("archer"), 3.0, 3.0)
      }
      if (piece.focus == "skirmisher" && turnNumber % 6 == 3) {
        setRandomTarget(piece)
        val _ = buildUnit(piece, externalInfo.pieceMap("skirmisher"), 3.0, 3.0)
      }
      if (piece.focus == "legionary" && turnNumber % 8 == 4) {
        setRandomTarget(piece)
        val _ = buildUnit(piece, externalInfo.pieceMap("legionary"), 4.0, 4.0)
      }
    }
    else if (piece.baseStats.name == "lair") {
      if (turnNumber <= 14 && turnNumber % 4 == 1) {
        setRandomTarget(piece)
        val rand = new Random(seed);
        seed = rand.nextInt(100000)
        val nextInt = rand.nextInt(4);
        if (nextInt == 0) {
          val _ = buildUnit(piece, externalInfo.pieceMap("archer"), 3.0, 3.0)
        }
        if (nextInt == 1) {
          val _ = buildUnit(piece, externalInfo.pieceMap("skirmisher"), 3.0, 3.0)
        }
        if (nextInt == 2) {
          val _ = buildUnit(piece, externalInfo.pieceMap("legionary"), 4.0, 4.0)
        }
        if (nextInt == 3) {
          val _ = buildUnit(piece, externalInfo.pieceMap("snake"), 4.0, 4.0)
        }  
      } 
      if (turnNumber > 14 && turnNumber % 4 == 1) {
        setRandomTarget(piece)
        val rand = new Random(seed);
        seed = rand.nextInt(100000)
        val nextInt = rand.nextInt(4);
        if (nextInt == 0) {
          val _ = buildUnit(piece, externalInfo.pieceMap("horseman"), 6.0, 6.0)
        }
        if (nextInt == 1) {
          val _ = buildUnit(piece, externalInfo.pieceMap("berserker"), 6.0, 6.0)
        }
        if (nextInt == 2) {
          val _ = buildUnit(piece, externalInfo.pieceMap("champion"), 8.0, 8.0)
        }
        if (nextInt == 3) {
          val _ = buildUnit(piece, externalInfo.pieceMap("crossbowman"), 8.0, 8.0)
        }                
      }
    }    
    else {
      // Not a city
      val poison = piece.modifiers._1
      val robustness = piece.baseStats.robust
      val damageFromPoison = Damage(Math.max(poison - robustness, 0))
      applyEffect(damageFromPoison, piece, externalInfo)
    }
  }

  private def refreshPieceForStartOfTurnWithAttackMove(piece: Piece, externalInfo: ExternalInfo): Unit = {
    refreshPieceForStartOfTurn(piece, externalInfo)
    attackMove(piece, externalInfo)
  }
}
