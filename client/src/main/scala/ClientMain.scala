import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.jquery.jQuery
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.html
import scala.util.Random
import scala.scalajs.js
import scala.util.{Try, Success, Failure}

import RichImplicits._

/**
  * PixelLoc, PixelVec:
  * The location of a pixel in normal canvas coordinates, and vectors for offsets between those locations.
  * As usual, (0,0) is the upper left corner, x is to the right, y is downward.
  *
  * Note: As these are doubles and capable of representing subpixel positions, these are actually locations of the
  * corners of pixels. For example, (14,52) is the upper left corner of a pixel, (14.5,52) is the midpoint of the top edge,
  * (14.5,52.5) is the center of the pixel, etc.
  */
object PixelLoc {
  def midpoint(a: PixelLoc, b: PixelLoc): PixelLoc = PixelLoc((a.x+b.x)/2.0,(a.y+b.y)/2.0)

  def ofLoc(loc : Loc, gridSize: Double) : PixelLoc = {
    val x = loc.x.toDouble
    val y = loc.y.toDouble
    PixelLoc(
      gridSize * Math.sqrt(3) * (x + y/2.0),
      gridSize * 3.0/2.0 * y
    )
  }
  def ofHexLoc(hexLoc : HexLoc, gridSize: Double) : PixelLoc = {
    val x = hexLoc.x
    val y = hexLoc.y
    PixelLoc(
      gridSize * Math.sqrt(3) * (x + y/2.0),
      gridSize * 3.0/2.0 * y
    )
  }
}
case class PixelLoc(x: Double, y: Double){
  def +(d: PixelVec): PixelLoc = PixelLoc(x + d.dx, y + d.dy)
  def -(d: PixelVec): PixelLoc = PixelLoc(x - d.dx, y - d.dy)
  def -(p: PixelLoc): PixelVec = PixelVec(x - p.x, y - p.y)
}
case class PixelVec(dx: Double, dy: Double){
  def +(p: PixelLoc): PixelLoc = PixelLoc(dx + p.x, dy + p.y)
  def +(d: PixelVec): PixelVec = PixelVec(dx + d.dx, dy + d.dy)
  def -(d: PixelVec): PixelVec = PixelVec(dx - d.dx, dy - d.dy)
}

/**
  * HexLoc, HexVec:
  * A floating point version of Loc and Vec from CommonTypes.scala. Integer coordinates correspond to the centers of hexes.
  *
  * x is up and to the right /
  * y is down |
  * z is up and to the left \
  *
  *        z 0  -1  -2   -3   -4
  *
  *    x 0   1   2   3   4      -5
  *  y   / \ / \ / \ / \ / \ y
  *   0 |   |   |   |   |   | 0   -6
  *      \ / \ / \ / \ / \ / \
  *     1 |   |   |   |   |   | 1   -7
  *        \ / \ / \ / \ / \ / \
  *       2 |   |   |   |   |   | 2   -8
  *          \ / \ / \ / \ / \ / \
  *         3 |   |   |   |   |   | 3
  *            \ / \ / \ / \ / \ / \
  *           4 |   |   |   |   |   | 4
  *              \ / \ / \ / \ / \ /
  *              x 0   1   2   3   4
  *
  *  This is consistent with HexTopology in CommonTypes.scala having (x-1,y-1) and (x+1,y+1) not be adjacent to (x,y),
  *  but all other (x + {-1,0,1}, y + {-1,0,1}) be adjacent.
  *
  *  Corner 0 = maximal x
  *  Corner 2 = maximal y
  *  Corner 4 = maximal z
  *  Hexant n = the triangle extending from the center out between corner n and corner n+1 mod 6
  *
  *  (x,y,z)
  *                            (corner 5)
  *                          (1/3,-2/3,1/3)
  *                                .
  *                            .       .
  *   (corner 4)           .       .       .           (corner 0)
  * (-1/3,-1/3,2/3)    .                       .     (2/3,-1/3,-1/3)
  *                .          4    .     5         .
  *                    .                       .
  *                .       .       .       .       .
  *                            .       .
  *                .     3         .         0     .
  *                            .       .
  *                .       .       .       .       .
  *                    .                       .
  *                .          2    .    1          .
  * (-2/3,1/3/1/3)     .                       .     (1/3,1/3,-2/3)
  *   (corner 3)           .       .       .           (corner 1)
  *                            .       .
  *                                .
  *                          (-1/3,2/3,-1/3)
  *                             (corner 2)
  */

object HexLoc {
  def midpoint(a: HexLoc, b: HexLoc): HexLoc = HexLoc((a.x+b.x)/2.0,(a.y+b.y)/2.0)

  def ofLoc(loc: Loc) = HexLoc(loc.x.toDouble,loc.y.toDouble)

  def ofPixel(p: PixelLoc, gridSize: Double): HexLoc = {
    HexLoc((p.x * Math.sqrt(3)/3 - p.y/3) / gridSize, p.y * 2.0/3.0 / gridSize)
  }

  //Returns the pixel loc corresponding to hexLoc + hexVec * scale
  def ofHexLocOffset(hexLoc : HexLoc, gridSize: Double, hexVec : HexVec, scale : Double) : PixelLoc = {
    val x = hexLoc.x + hexVec.dx * scale
    val y = hexLoc.y + hexVec.dy * scale
    PixelLoc(
      gridSize * Math.sqrt(3) * (x + y/2.0),
      gridSize * 3.0/2.0 * y
    )
  }
}
case class HexLoc(x: Double, y: Double) {
  def z: Double = -(x+y)
  def +(d: HexVec): HexLoc = HexLoc(x + d.dx, y + d.dy)
  def -(d: HexVec): HexLoc = HexLoc(x - d.dx, y - d.dy)
  def -(p: HexLoc): HexVec = HexVec(x - p.x, y - p.y)
  def -(p: Loc): HexVec = HexVec(x - p.x, y - p.y)

  //Find the Loc for the hex that contains this HexLoc, taking into account the hexagon shape of a cell.
  def round(): Loc = {
    val rx = Math.round(x)
    val ry = Math.round(y)
    val rz = Math.round(z)
    val dx = Math.abs(x - rx)
    val dy = Math.abs(y - ry)
    val dz = Math.abs(z - rz)
    if(dx >= dy && dx >= dz)
      Loc(-(ry+rz).toInt, ry.toInt)
    else if(dz >= dx && dz >= dy)
      Loc(rx.toInt, ry.toInt)
    else // (dz >= dx && dz>=dy);
      Loc(rx.toInt, -(rx+rz).toInt)
  }
}

object HexVec {
  val corners: Array[HexVec] = {
    val third: Double = 1.0 / 3.0
    Array(
      HexVec( 2*third, -1*third),
      HexVec( 1*third,  1*third),
      HexVec(-1*third,  2*third),
      HexVec(-2*third,  1*third),
      HexVec(-1*third, -1*third),
      HexVec( 1*third, -2*third)
    )
  }
}
case class HexVec(dx: Double, dy: Double){
  def dz: Double = -(dx+dy)
  def +(p: HexLoc): HexLoc = HexLoc(dx + p.x, dy + p.y)
  def +(d: HexVec): HexVec = HexVec(dx + d.dx, dy + d.dy)
  def -(d: HexVec): HexVec = HexVec(dx - d.dx, dy - d.dy)
  def *(c: Double): HexVec = HexVec(dx*c,dy*c)

  //Determine which hexant this belongs to
  def hexant(): Int = {
    val moreLeftThanRight = dz > dx
    val moreURThanDL = dx > dy
    val moreDRthanUL = dy > dz

    (moreLeftThanRight, moreURThanDL, moreDRthanUL) match {
      case (false,false,false) => 0 //Zero vector
      case (false,false,true) => 1
      case (false,true,false) => 5
      case (false,true,true) => 0
      case (true,false,false) => 3
      case (true,false,true) => 2
      case (true,true,false) => 4
      case (true,true,true) => 0 //Shouldn't happen
    }
  }

  //Determine which hexant this belongs to
  def closestCorner(): Int = {
    (dx > 0, dy > 0, dz > 0) match {
      case (false,false,false) => 0 //Zero vector
      case (false,false,true) => 4
      case (false,true,false) => 2
      case (false,true,true) => 3
      case (true,false,false) => 0
      case (true,false,true) => 5
      case (true,true,false) => 1
      case (true,true,true) => 0 //Shouldn't happen
    }
  }
}
//TODO dwu: Consider having an implicit HexLoc => Loc


//TODO dwu: At some point we probably want to split things up into multiple files. Maybe a reasonable separation would be
//to factor out the drawing logic from the user-input-handling.

object ClientMain extends JSApp {
  //The euclidean distance from the center of a hexagon to the corner in pixels.
  val gridSize = 30.0
  val tileScale = 29.0 / gridSize
  val pieceScale = 25.0 / gridSize
  val smallPieceScale = 14.0 / gridSize
  val smallPieceOffset = 15.0 / gridSize

  def move(ctx : CanvasRenderingContext2D, loc : Loc) : Unit = {
    move(ctx,HexLoc.ofLoc(loc))
  }
  def move(ctx : CanvasRenderingContext2D, hexLoc : HexLoc) : Unit = {
    val pixel = PixelLoc.ofHexLoc(hexLoc,gridSize)
    ctx.moveTo(Math.floor(pixel.x), Math.floor(pixel.y));
  }
  def line(ctx : CanvasRenderingContext2D, loc : Loc) : Unit = {
    line(ctx,HexLoc.ofLoc(loc))
  }
  def line(ctx : CanvasRenderingContext2D, hexLoc : HexLoc) : Unit = {
    val pixel = PixelLoc.ofHexLoc(hexLoc,gridSize)
    ctx.lineTo(Math.floor(pixel.x), Math.floor(pixel.y));
  }
  def text(ctx : CanvasRenderingContext2D, text : String, pixel : PixelLoc, color : String) : Unit = {
    ctx.globalAlpha = 1.0
    ctx.fillStyle = color
    ctx.textAlign = "center"
    ctx.fillText(text, Math.floor(pixel.x), Math.floor(pixel.y))
  }

  def hexCorner(hexLoc : HexLoc, scale : Double, corner : Int) : HexLoc = {
    hexLoc + HexVec.corners(corner) * scale
  }

  def drawHex(ctx : CanvasRenderingContext2D, loc : Loc, color : String, scale : Double) : Unit = {
    drawHex(ctx,HexLoc.ofLoc(loc), color, scale)
  }
  def drawHex(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, color : String, scale : Double) : Unit = {
    ctx.globalAlpha = 0.2
    ctx.fillStyle = color
    ctx.beginPath()
    move(ctx, hexLoc + HexVec.corners(0) * scale)
    line(ctx, hexLoc + HexVec.corners(1) * scale)
    line(ctx, hexLoc + HexVec.corners(2) * scale)
    line(ctx, hexLoc + HexVec.corners(3) * scale)
    line(ctx, hexLoc + HexVec.corners(4) * scale)
    line(ctx, hexLoc + HexVec.corners(5) * scale)
    line(ctx, hexLoc + HexVec.corners(0) * scale)
    ctx.fill();
    ctx.closePath();
  }

  def drawTile(ctx : CanvasRenderingContext2D, loc : Loc, tile: Tile) : Unit = {
    drawTile(ctx,HexLoc.ofLoc(loc),tile)
  }
  def drawTile(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, tile: Tile) : Unit = {
    tile.terrain match {
      case Wall => drawHex(ctx, hexLoc, "black", tileScale)
      case Ground => drawHex(ctx, hexLoc, "green", tileScale)
      case Water => drawHex(ctx, hexLoc, "blue", tileScale)
      case ManaSpire => drawHex(ctx, hexLoc, "orange", tileScale)
      case Spawner(S0, _) =>
        drawHex(ctx, hexLoc, "gray", tileScale)
        drawHex(ctx, hexLoc, "red", tileScale*0.7)
      case Spawner(S1, _) =>
        drawHex(ctx, hexLoc, "gray", tileScale)
        drawHex(ctx, hexLoc, "blue", tileScale*0.7)
    }
  }

  def drawPiece(ctx : CanvasRenderingContext2D, loc : Loc, scale : Double, piece: Piece, isSelected: Boolean) : Unit = {
    drawPiece(ctx,HexLoc.ofLoc(loc),scale,piece,isSelected)
  }
  def drawPiece(ctx : CanvasRenderingContext2D, hexLoc : HexLoc, scale : Double, piece: Piece, isSelected: Boolean) : Unit = {
    def pieceColor(p : Piece, isSelected: Boolean) : String = {
      if(isSelected) "red" else "blue"
    }
    drawHex(ctx, hexLoc, pieceColor(piece,isSelected), scale)
    text(ctx, piece.id.toString, PixelLoc.ofHexLoc(hexLoc,gridSize), "black")
  }

  def main(): Unit = {
    //TODO from dwu to jpaulson: I'm not sure I understand what "setupUI _" means. Could you explain to me?
    jQuery(setupUI _)
    ()
  }

  def setupUI() : Unit = {
    val canvas = jQuery("#board").get(0).asInstanceOf[html.Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    //How much to translate the canvas origin inward from the upper left corner.
    val translateOrigin = PixelVec(2.0*gridSize, 2.0*gridSize)

    val board = BoardState.create(Plane.create(10, 10, HexTopology, Ground))

    var selected : Option[Piece] = None
    var mouse = Loc(0, 0)
    var path = List(Loc(0, 0))

    //TODO dwu: Eventually this will need to take into account traversibility.
    //At that point, it will also need to be a true recursive search rather than this greedy algorithm.

    // Update path to be a shortest path from [selected] to [mouse] that
    // shares the longest prefix with the current [path]
    def updatePath() : Unit = {
      def distance(x : Loc, y : Loc) : Int = {
        board.tiles.topology.distance(x, y)
      }
      selected match {
        case None => ()
        case Some(p) =>
          val loc = p.loc
          //TODO from dwu to jpaulson: In scala, ".equals" is the same as "==", so just use "==" or "!=". Unlike ocaml, this will call out
          //to the object-specific comparison method so it's right even for things like Maps with non-semantic internal structure.
          if(path.length==0 || (!path(0).equals(loc))) {
            path = List(loc)
          }
          while(path.length > 0 && distance(loc, mouse) != path.length-1 + distance(path(path.length-1), mouse)) {
            path = path.init
          }
          if(path(path.length-1) != mouse) {
            for(p <- board.tiles.topology.adj(path(path.length-1))) {
              if(path.length-1 + distance(path(path.length-1), mouse) == path.length + distance(p, mouse)) {
                path = path :+ p
              }
            }
          }
      }
    }

    def isPieceSelected(piece: Piece) : Boolean = {
      selected match {
        case None => false
        case Some(p) => piece.id == p.id
      }
    }

    def showBoard(board : BoardState) : Unit = {
      ctx.setTransform(1,0,0,1,0,0)
      ctx.clearRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)
      ctx.translate(translateOrigin.dx,translateOrigin.dy)
      drawHex(ctx, mouse, "purple", tileScale)
      selected match {
        case None => ()
        case Some(p) =>
          drawHex(ctx, p.loc, "red", tileScale)
      }
      board.tiles.iteri {case (loc, tile) =>
        drawTile(ctx,loc,tile)
      }
      board.pieces.iteri {case (loc, pieces) =>
        pieces match {
          case Nil => ()
          case p :: Nil =>
            drawPiece(ctx, loc, pieceScale, p, isPieceSelected(p))
          case p1 :: p2 :: Nil  =>
            val c1 = HexLoc.ofLoc(loc) + HexVec.corners(5) * smallPieceOffset
            val c2 = HexLoc.ofLoc(loc) + HexVec.corners(2) * smallPieceOffset
            drawPiece(ctx, c1, smallPieceScale, p1, isPieceSelected(p1))
            drawPiece(ctx, c2, smallPieceScale, p2, isPieceSelected(p2))
          case p1 :: p2 :: p3 :: Nil  => ()
            val c1 = HexLoc.ofLoc(loc) + HexVec.corners(5) * smallPieceOffset
            val c2 = HexLoc.ofLoc(loc) + HexVec.corners(3) * smallPieceOffset
            val c3 = HexLoc.ofLoc(loc) + HexVec.corners(1) * smallPieceOffset
            drawPiece(ctx, c1, smallPieceScale, p1, isPieceSelected(p1))
            drawPiece(ctx, c2, smallPieceScale, p2, isPieceSelected(p2))
            drawPiece(ctx, c3, smallPieceScale, p3, isPieceSelected(p3))
          case _ => ()
        }
      }
      if(path.length > 0) {
        ctx.globalAlpha = 1.0
        ctx.fillStyle = "black"
        ctx.setLineDash(js.Array(5.0, 10.0))
        ctx.beginPath()
        move(ctx, path(0))
        for(i <- 1 to path.length-1) {
          line(ctx, path(i))
        }
        ctx.stroke()
        ctx.closePath()
      }
    }

    def mousePixel(e : MouseEvent) : PixelLoc = {
      val rect = canvas.getBoundingClientRect()
      PixelLoc(e.clientX - rect.left, e.clientY - rect.top) - translateOrigin
    }
    def mouseHexLoc(e : MouseEvent) : HexLoc = {
      HexLoc.ofPixel(mousePixel(e), gridSize)
    }

    def mousePiece(e : MouseEvent) : Option[Piece] = {
      val hexLoc = mouseHexLoc(e)
      val loc = hexLoc.round()
      val hexDelta = hexLoc - loc

      board.pieces(loc) match {
        case Nil => None
        case p :: Nil => Some(p)
        case p1 :: p2 :: Nil =>
          hexDelta.closestCorner() match {
            case 0 | 4 | 5 => Some(p1)
            case 1 | 2 | 3 => Some(p2)
          }
        case p1 :: p2 :: p3 :: Nil =>
          hexDelta.hexant() match {
            case 4 | 5 => Some(p1)
            case 2 | 3 => Some(p2)
            case 0 | 1 => Some(p3)
            case _ => assertUnreachable()
          }
        case _ => None
      }
    }

    def mousedown(e : MouseEvent) : Unit = {
      selected match {
        case None =>
          selected = mousePiece(e)
        case Some(piece) =>
          val action = Movements(List(Movement(StartedTurnWithID(piece.id), path.toArray)))
          val result = board.doAction(action)
          selected = None
          path = List()
          result match {
            case Success(_) => ()
            case Failure(error) =>
              println(error)
              selected = mousePiece(e)
          }
      }
      showBoard(board)
    }
    def mousemove(e : MouseEvent) : Unit = {
      mouse = mouseHexLoc(e).round()
      updatePath()
      showBoard(board)
    }

    canvas.onmousedown = mousedown _
    canvas.onmousemove = mousemove _

    val zombie = PieceStats(
      name = "zombie",
      displayName = "Zombie",
      attackEffect = Some(Damage(1)),
      defense = 2,
      moveRange = 100,
      attackRange = 1,
      cost = 2,
      rebate = 0,
      isNecromancer = false,
      isFlying = false,
      isLumbering = true,
      isPersistent = false,
      isEldritch = false,
      isWailing = false,
      hasFlurry = false,
      hasBlink = false,
      canHurtNecromancer = true,
      swarmMax = 3, //Swarming 3 for testing
      spawnRange = 0,
      extraMana = 0,
      deathSpawn = None,
      freeSpawn = None,
      abilities = Map.empty
    )

    //TODO from dwu to jpaulson: board.tiles(x,y) = z   -  syntatic sugar converts this into .update
    //TODO from dwu to jpaulson: Don't mutate directly, instead modify the terrain Plane to be what you want first
    //and then create the BoardState with the desired terain.
    board.tiles.update(0, 0, Tile.create(ManaSpire))
    board.tiles.update(0, 1, Tile.create(Wall))
    board.tiles.update(1, 0, Tile.create(Water))
    board.tiles.update(1, 1, Tile.create(Spawner(S0, zombie)))
    board.tiles.update(2, 0, Tile.create(Spawner(S1, zombie)))

    board.spawnPieceInitial(S0, zombie, Loc(2, 1))
    board.spawnPieceInitial(S0, zombie, Loc(2, 2))
    board.spawnPieceInitial(S0, zombie, Loc(2, 2))

    board.spawnPieceInitial(S0, zombie, Loc(2, 3))
    board.spawnPieceInitial(S0, zombie, Loc(2, 3))
    board.spawnPieceInitial(S0, zombie, Loc(2, 3))

    showBoard(board)
    ()
  }

  def addClickedMessage(): Unit = {
    jQuery("body").append("<p>You clicked!</p>")
    ()
  }
}
