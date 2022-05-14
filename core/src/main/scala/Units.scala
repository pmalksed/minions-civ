package minionsgame.core
import scala.collection.immutable.Map
import scala.util.{Try,Success,Failure}

object Units {
  def fromForm(
    name: String,
    attack: String,
    health: String,
    speed: String,
    range: String,
    cost: String,
    rebate: String,
    numAttacks: String,
    swarm: Option[String],
    lumbering: Option[String],
    spawn: Option[String],
    persistent: Option[String],
    flying: Option[String],
    blink: Option[String],
    ability: String,
  ): Option[PieceStats] = {
    if(name=="") {
      None
    } else {
      val attackEffect =
        attack match {
          case "*" => Some(Unsummon)
          case "Deadly" => Some(Kill)
          case "deadly" => Some(Kill)
          case _ => Some(Damage(attack.toInt))
        }
      val numAttacksInt = if(numAttacks.isEmpty) 1 else numAttacks.toInt
      val rebateInt =
        if(rebate forall Character.isDigit) rebate.toInt else 0
      val deathSpawn =
        if(rebate forall Character.isDigit) None else Some(rebate)
      val swarmMax = if(swarm.isDefined) 3 else 1
      val isLumbering = if(lumbering.isDefined) true else false
      val spawnRange = if(spawn.isDefined) Some(1) else None
      val isPersistent = if(persistent.isDefined) true else false
      val isFlying = if(flying.isDefined) true else false
      val canBlink = if(blink.isDefined) true else false
      val abilities =
        if(!Abilities.abilityMap.contains(ability)) List() else List(Abilities.abilityMap(ability))
      Some(createPieceStats(name=name, shortDisplayName=name, displayName=name, attackEffect=attackEffect,
        defense=Some(health.toInt), moveRange=speed.toInt, attackRange=range.toInt, numAttacks=numAttacksInt,
        cost=cost.toInt, rebate=rebateInt, deathSpawn=deathSpawn, swarmMax=swarmMax, isLumbering=isLumbering,
        spawnRange=spawnRange, isPersistent=isPersistent, isFlying=isFlying, canBlink=canBlink, abilities=abilities))
    }
  }
  def createPieceStats(
    name : String,
    shortDisplayName: String = "",
    displayName: String = "",
    attackEffect : Option[TargetEffect],
    defense : Option[Int],
    moveRange : Int,
    attackRange : Int,
    attackRangeVsFlying: Int = 0,
    numAttacks : Int = 1,
    cost : Int = 1,
    rebate : Int = 1,
    isNecromancer : Boolean = false,
    isFlying : Boolean = false,
    isLumbering : Boolean = false,
    isPersistent : Boolean = false,
    isEldritch : Boolean = false,
    isSoulbound : Boolean = false,
    isWailing : Boolean = false,
    canBlink : Boolean = false,
    canHurtNecromancer : Boolean = true,
    swarmMax : Int = 1,
    spawnRange : Option[Int] = None,
    extraSouls : Int = 0,
    extraMana : Int = 0,
    deathSpawn : Option[PieceName] = None,
    perTurnReinforcement : Option[PieceName] = None,
    abilities : List[PieceAbility] = List.empty,
    productionCost: Int = 3,
    scienceCost: Int = 6,
    notes: String = "",
    notes2: String = "",
    nimble: Boolean = false,
    charge: Boolean = false,
    retaliate: Boolean = false,
    poisonous: Int = 0,
    robust: Int = 0,
    leadership: Boolean = false,
    taunt: Boolean = false,
  ) : PieceStats = {
    PieceStats(
      name = name,
      shortDisplayName = (if(shortDisplayName == "") name.capitalize else shortDisplayName),
      displayName = (if(displayName == "") name.capitalize else displayName),
      isBaseStats = true,
      attackEffect = attackEffect,
      defense = defense,
      moveRange = moveRange,
      attackRange = attackRange,
      attackRangeVsFlying = Math.max(attackRange,attackRangeVsFlying),
      numAttacks = numAttacks,
      cost = cost,
      rebate = rebate,
      isNecromancer = isNecromancer,
      isFlying = isFlying,
      isLumbering = isLumbering,
      isPersistent = isPersistent,
      isEldritch = isEldritch,
      isSoulbound = isSoulbound,
      isWailing = isWailing,
      canBlink = canBlink,
      canHurtNecromancer = canHurtNecromancer,
      swarmMax = swarmMax,
      spawnRange = spawnRange,
      extraSouls = extraSouls,
      extraMana = extraMana,
      deathSpawn = deathSpawn,
      perTurnReinforcement = perTurnReinforcement,
      abilities = abilities,
      productionCost = productionCost,
      scienceCost = scienceCost,
      notes = notes,
      notes2 = notes2,
      nimble = nimble,
      charge = charge,
      retaliate = retaliate,
      poisonous = poisonous,
      robust = robust,
      leadership = leadership,
      taunt = taunt,
    )
  }

  val necromancer = createPieceStats(
    name = "necromancer",
    shortDisplayName = "Necro",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Unsummon),
    defense = Some(7),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3
  )
  val mana_necromancer = createPieceStats(
    name = "mana_necromancer",
    displayName = "ManaNecromancer",
    shortDisplayName = "ManaNe",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Unsummon),
    defense = Some(10),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3,
    extraMana = 1
  )
  val battle_necromancer = createPieceStats(
    name = "battle_necromancer",
    displayName = "FlurryNecromancer",
    shortDisplayName = "FlurryNec",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Damage(1)),
    numAttacks = 4,
    defense = Some(10),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3
  )
  val swarm_necromancer = createPieceStats(
    name = "swarm_necromancer",
    shortDisplayName = "SNec",
    displayName = "Swarm Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Unsummon),
    defense = Some(10),
    spawnRange = Some(1),
    isPersistent = true,
    swarmMax = 3,
    isNecromancer = true,
    extraSouls = 1,
  )
  val arcane_necromancer = createPieceStats(
    name = "arcane_necromancer",
    shortDisplayName = "ArcNec",
    displayName = "Arcane Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Unsummon),
    defense = Some(7),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 4,
  )
  val mounted_necromancer = createPieceStats(
    name = "mounted_necromancer",
    shortDisplayName = "MntNec",
    displayName = "Mounted Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 2,
    attackRange = 0,
    attackEffect = None,
    defense = Some(10),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3,
  )
  val ranged_necromancer = createPieceStats(
    name = "ranged_necromancer",
    shortDisplayName = "RngNec",
    displayName = "Ranged Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 3,
    attackEffect = Some(Unsummon),
    defense = Some(10),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3,
  )
  val deadly_necromancer = createPieceStats(
    name = "deadly_necromancer",
    shortDisplayName = "DedNec",
    displayName = "Deadly Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Kill),
    defense = Some(10),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3,
    canHurtNecromancer = false,
  )
  val immortal_necromancer = createPieceStats(
    name = "immortal_necromancer",
    shortDisplayName = "ImmNec",
    displayName = "Immortal Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    isFlying = true,
    attackRange = 1,
    attackEffect = Some(Unsummon),
    defense = None,
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3,
  )
  val summoner_necromancer = createPieceStats(
    name = "summoner_necromancer",
    shortDisplayName = "SumNec",
    displayName = "Summoner Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Unsummon),
    defense = Some(7),
    spawnRange = Some(2),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3,
  )

  val zombie = createPieceStats(
    name = "zombie",
    cost = 2,
    rebate = 0,
    moveRange = 1,
    isLumbering = true,
    attackRange = 1,
    attackEffect = Some(Damage(1)),
    defense = Some(2),
  )

  val zombie_necromancer = createPieceStats(
    name = "zombie_necromancer",
    shortDisplayName = "ZomNec",
    displayName = "Zombie Necromancer",
    cost = 0,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Unsummon),
    defense = Some(10),
    spawnRange = Some(1),
    isPersistent = true,
    isNecromancer = true,
    extraSouls = 3,
    perTurnReinforcement = Some(zombie.name)
  )

  val acolyte = createPieceStats(
    name = "acolyte",
    cost = 5,
    rebate = 3,
    moveRange = 2,
    attackRange = 0,
    attackEffect = None,
    defense = Some(2),
    spawnRange = Some(1)
  )

  val spire = createPieceStats(
    name = "spire",
    cost = 4,
    rebate = 1,
    moveRange = 0,
    attackRange = 1,
    attackEffect = Some(Damage(5)),
    defense = Some(4),
    spawnRange = Some(1),
  )

  val salvager = createPieceStats(
    name = "salvager",
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Damage(0)),
    defense = Some(5),
    productionCost = 5,
    scienceCost = 4,
    notes = "Picks up resources from dead units and carries them to your cities to be recycled",
    notes2 = "Has 5 defense and 5 capacity by default and gets +1 of each per Salvager building you build",
  )

  val warrior = createPieceStats(
    name = "warrior",
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Damage(3)),
    defense = Some(6),
    productionCost = 2,
    scienceCost = 0,
    notes = "Can be built without a corresponding building"
  )

  val archer = createPieceStats(
    name = "archer",
    moveRange = 1,
    attackRange = 2,
    attackEffect = Some(Damage(3)),
    defense = Some(4),
    productionCost = 3,
    scienceCost = 4,
  )

  val skirmisher = createPieceStats(
    name = "skirmisher",
    moveRange = 1,
    charge = true,
    nimble = true,
    attackRange = 1,
    attackEffect = Some(Damage(2)),
    defense = Some(5),
    productionCost = 1,
    scienceCost = 5,
  )

  val legionary = createPieceStats(
    name = "legionary",
    moveRange = 1,
    retaliate = true,
    attackRange = 1,
    attackEffect = Some(Damage(3)),
    defense = Some(9),
    productionCost = 5,
    scienceCost = 5,
  )

  val snake = createPieceStats(
    name = "snake",
    moveRange = 1,
    attackRange = 2,
    attackEffect = Some(Damage(1)),
    poisonous = 1,
    numAttacks = 5,
    defense = Some(4),
    productionCost = 4,
    scienceCost = 6,
  )

  val horseman = createPieceStats(
    name = "horseman",
    charge = true,
    moveRange = 2,
    attackRange = 1,
    attackEffect = Some(Damage(4)),
    defense = Some(7),
    productionCost = 6,
    scienceCost = 8,
  )

  val berserker = createPieceStats(
    name = "berserker",
    charge = true,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Damage(5)),
    defense = Some(6),
    robust = 1,
    productionCost = 5,
    scienceCost = 10,
  )

  val trebuchet = createPieceStats(
    name = "trebuchet",
    moveRange = 1,
    attackRange = 3,
    attackEffect = Some(Damage(2)),
    defense = Some(4),
    productionCost = 6,
    scienceCost = 3,
    notes = "Deals triple damage to cities"
  )

  val sergeant = createPieceStats(
    name = "sergeant",
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Damage(2)),
    defense = Some(7),
    leadership = true,
    productionCost = 7,
    scienceCost = 6,
    notes = "Nearby friendly units get +1 attack"
  )

  val champion = createPieceStats(
    name = "champion",
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Damage(7)),
    defense = Some(9),
    retaliate = true,
    productionCost = 7,
    scienceCost = 12,
  )

  val telekinetic = createPieceStats(
    name = "telekinetic",
    moveRange = 1,
    attackRange = 3,
    attackEffect = Some(Damage(2)),
    defense = Some(5),
    productionCost = 8,    
    scienceCost = 8,
    notes = "Does triple damage to salvagers",
    notes2 = "Steals stored resources equal to damage dealt and drops them",
  )

  val stalwart = createPieceStats(
    name = "stalwart",
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Damage(2)),
    defense = Some(12),
    robust = 1,
    taunt = true,
    productionCost = 6,
    scienceCost = 8,
  )

  val crossbowman = createPieceStats(
    name = "crossbowman",
    moveRange = 1,
    attackRange = 2,
    attackEffect = Some(Damage(5)),
    defense = Some(6),
    productionCost = 6,
    scienceCost = 10,
  )

  val mule = createPieceStats(
    name = "mule",
    moveRange = 2,
    attackRange = 1,
    attackEffect = Some(Damage(0)),
    defense = Some(8),
    productionCost = 5,
    scienceCost = 3,
    notes = "Unaffected by the Suicide Tax"
  )

  val longbowman = createPieceStats(
    name = "longbowman",
    moveRange = 1,
    attackRange = 3,
    attackEffect = Some(Damage(3)),
    defense = Some(5),
    nimble = true,
    productionCost = 7,
    scienceCost = 14,
  )

  val hussar = createPieceStats(
    name = "hussar",
    moveRange = 2,
    attackRange = 1,
    attackEffect = Some(Damage(3)),
    defense = Some(7),
    nimble = true,
    productionCost = 7,
    scienceCost = 12,
    notes = "Ignores enemey melee units"
  )

  val wraith = createPieceStats(
    name = "wraith",
    cost = 6,
    rebate = 2,
    moveRange = 3,
    isFlying = true,
    isLumbering = true,
    attackRange = 2,
    attackEffect = Some(Damage(4)),
    defense = Some(8),
    isPersistent = true,
  )

  val fiend = createPieceStats(
    name = "fiend",
    cost = 3,
    rebate = 0,
    moveRange = 1,
    attackRange = 1,
    attackEffect = Some(Damage(1)),
    numAttacks = 8,
    defense = Some(6)
  )

  val banshee = createPieceStats(
    name = "banshee",
    cost = 4,
    rebate = 1,
    moveRange = 2,
    attackRange = 1,
    attackEffect = Some(Kill),
    defense = Some(2),
    isPersistent = true,
    canHurtNecromancer = false
  )

  val elemental = createPieceStats(
    name = "elemental",
    shortDisplayName = "Element",
    cost = 8,
    rebate = 5,
    moveRange = 1,
    attackRange = 3,
    attackEffect = Some(Damage(3)),
    defense = Some(2),
    abilities = List(MoveTerrain)
  )

  val fallen_angel = createPieceStats(
    name = "fallen_angel",
    shortDisplayName = "FAngel",
    displayName = "Fallen Angel",
    cost = 7,
    rebate = 4,
    moveRange = 2,
    isFlying = true,
    attackRange = 2,
    attackEffect = Some(Damage(2)),
    defense = Some(4),
    spawnRange = Some(1),
  )

  val shadowlord = createPieceStats(
    name = "shadowlord",
    shortDisplayName = "SLord",
    cost = 10,
    rebate = 5,
    moveRange = 2,
    isFlying = true,
    attackRange = 1,
    attackEffect = Some(Damage(8)),
    defense = Some(10),
    isPersistent = true,
  )

  val city = createPieceStats(
    name = "city",
    shortDisplayName = "City",
    cost = 0,
    rebate = 0,
    moveRange = 0,
    attackRange = 2,
    attackEffect = Some(Damage(3)),
    defense = Some(15),
  )

  //All pieces
  val pieces: Array[PieceStats] = Array(
    necromancer,
    arcane_necromancer,
    ranged_necromancer,
    mounted_necromancer,
    immortal_necromancer,
    deadly_necromancer,
    battle_necromancer,
    zombie_necromancer,
    zombie, acolyte,
    salvager, warrior, archer, skirmisher, legionary, snake, horseman, berserker,
    trebuchet, sergeant, champion, telekinetic, stalwart, crossbowman, mule, longbowman, hussar,
    wraith, fiend, banshee, elemental, fallen_angel, shadowlord, city
  )

  //Necromancers awarded after a board resets
  val specialNecromancers: Array[PieceStats] = Array(
    arcane_necromancer,
    ranged_necromancer,
    mounted_necromancer,
    immortal_necromancer,
    deadly_necromancer,
    battle_necromancer,
    zombie_necromancer,
    //mana_necromancer,
    //swarm_necromancer,
    //summoner_necromancer,
  )

  //Pieces that start off buyable
  val alwaysAcquiredPieces: Array[PieceStats] = Array(
    zombie,
    acolyte,
  )

  //Pieces that need to be unlocked, in order
  val techPieces: Array[PieceStats] = Array(
    salvager,
    warrior,
    archer,
    skirmisher,
    legionary,
    snake,
    horseman,
    berserker,
    trebuchet,
    sergeant,
    champion,
    telekinetic,
    stalwart,
    crossbowman,
    mule,
    longbowman,
    hussar,
    wraith,
    fiend,
    banshee,
    elemental,
    fallen_angel,
    shadowlord
  )

  //Given a piece, get its index within the pieces array
  val allPiecesIdx: Map[PieceName,Int] =
    pieces.zipWithIndex.groupBy { case (piece,_) => piece.name }.mapValues { grouped =>
      assert(grouped.length == 1)
      grouped.head._2
    }

  //Generally, we store and send the PieceName everywhere in the protocol, since unlike a PieceStats it's easily serialized.
  //This is the global map that everything uses to look up the stats again from the name.
  val pieceMap: Map[PieceName,PieceStats] =
  pieces.groupBy(piece => piece.name).mapValues { pieces =>
    assert(pieces.length == 1)
    pieces.head
  }

}
