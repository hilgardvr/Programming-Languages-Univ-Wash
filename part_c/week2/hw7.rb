        # University of Washington, Programming Languages, Homework 7, hw7.rb 
# (See also ML code)

# a little language for 2D geometry objects

# each subclass of GeometryExpression, including subclasses of GeometryValue,
#  needs to respond to messages preprocess_prog and eval_prog
#
# each subclass of GeometryValue additionally needs:
#   * shift
#   * intersect, which uses the double-dispatch pattern
#   * intersectPoint, intersectLine, and intersectVerticalLine for 
#       for being called by intersect of appropriate clases and doing
#       the correct intersection calculuation
#   * (We would need intersectNoPoints and intersectLineSegment, but these
#      are provided by GeometryValue and should not be overridden.)
#   *  intersectWithSegmentAsLineResult, which is used by 
#      intersectLineSegment as described in the assignment
#
# you can define other helper methods, but will not find much need to

# Note: geometry objects should be immutable: assign to fields only during
#       object construction

# Note: For eval_prog, represent environments as arrays of 2-element arrays
# as described in the assignment

class GeometryExpression  
  # do *not* change this class definition
  Epsilon = 0.00001
end

class GeometryValue 
  # do *not* change methods in this class definition
  # you can add methods if you wish

  private
  # some helper methods that may be generally useful
  def real_close(r1,r2) 
    (r1 - r2).abs < GeometryExpression::Epsilon
  end
  def real_close_point(x1,y1,x2,y2) 
    real_close(x1,x2) && real_close(y1,y2)
  end
  # two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1,y1,x2,y2) 
    if real_close(x1,x2)
      VerticalLine.new x1
    else
      m = (y2 - y1).to_f / (x2 - x1)
      b = y1 - m * x1
      Line.new(m,b)
    end
  end

  public
  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints np
    np # could also have NoPoints.new here instead
  end

  # we put this in this class so all subclasses can inhert it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
end

class NoPoints < GeometryValue
  # do *not* change this class definition: everything is done for you
  # (although this is the easiest class, it shows what methods every subclass
  # of geometry values needs)
  # However, you *may* move methods from here to a superclass if you wish to

  # Note: no initialize method only because there is nothing it needs to do
  def eval_prog env 
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    self # shifting no-points is no-points
  end
  def intersect other
    other.intersectNoPoints self # will be NoPoints but follow double-dispatch
  end
  def intersectPoint p
    self # intersection with point and no-points is no-points
  end
  def intersectLine line
    self # intersection with line and no-points is no-points
  end
  def intersectVerticalLine vline
    self # intersection with line and no-points is no-points
  end
  # if self is the intersection of (1) some shape s and (2) 
  # the line containing seg, then we return the intersection of the 
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    self
  end
end


class Point < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods

  # Note: You may want a private helper method like the local
  # helper function inbetween in the ML code
  attr_reader :x, :y
  def initialize(x,y)
    @x = x
    @y = y
  end

  def preprocess_prog
    self
  end

  def eval_prog env
    self
  end

  def shift(dx, dy)
   self.class.new(@x + dx, @y + dy)
  end 

  def intersect other
    other.intersectPoint self.class.new(@x, @y)
  end 

  def intersectPoint p
    if real_close_point(p.x, p.y, @x, @y)
      then p
      else NoPoints.new
    end
  end

  def intersectLine other
   if real_close(@y, other.m * @x + other.b)
     then self.class.new(@x, @y)
     else NoPoints.new
   end
  end

  def intersectVerticalLine other
    if real_close(@x, other.x)
      then self.class.new(@x, @y)
      else NoPoints.new
    end
  end

  private def inbetween(v, end1, end2)
    ((((end1 - GeometryExpression::Epsilon) <= v) && (v <= (end2 + GeometryExpression::Epsilon))) || (((end2 - GeometryExpression::Epsilon) <= v) && (v <= (end1 + GeometryExpression::Epsilon))))
  end

  def intersectWithSegmentAsLineResult seg
    if (inbetween(@x, seg.x1, seg.x2) and inbetween(@y, seg.y1, seg.y2))
    then self.class.new(@x, @y)
    else NoPoints.new
    end
  end

end

class Line < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b 
  def initialize(m,b)
    @m = m
    @b = b
  end

  def preprocess_prog
    self
  end

  def eval_prog env
    self
  end

  def shift(dx, dy)
   self.class.new(@m, @b + dy - (@m * dx))
  end 

  def intersect other
    other.intersectLine self.class.new(@m, @b)
  end 

  def intersectPoint p
   if real_close(p.y, @m * p.x + @b)
     then Point.new(p.x, p.y)
     else NoPoints.new
   end
  end

  def intersectLine other
    if real_close(other.m, @m)
      then
        if real_close(other.b, @b)
          then self.class.new(@m, @b)
          else NoPoints.new
        end
      else
        x = (other.b - @b) / (@m - other.m)
        y = @m * x + @b   
        Point.new(x, y)
    end
  end

  def intersectVerticalLine other
    Point.new(other.x, @m * other.x + @b)
  end

  def intersectWithSegmentAsLineResult seg
    seg 
  end

end

class VerticalLine < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x
  def initialize x
    @x = x
  end

  def preprocess_prog
    self
  end

  def eval_prog env
    self
  end

  def shift(dx, dy)
   self.class.new(@x + dx)
  end 

  def intersect other
    other.intersectVertical self.class.new(@x)
  end 

  def intersectPoint p
    if real_close(p.x, @x)
      then p
      else NoPoints.new
    end
  end

  def intersectLine other
    Point.new(@x, other.m * @x + other.b)
  end

  def intersectVerticalLine other
    if real_close(@x, other.x)
      then self.class.new(@x)
      else NoPoints.new
    end
  end

  def intersectWithSegmentAsLineResult seg
    seg
  end
end

class LineSegment < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: This is the most difficult class.  In the sample solution,
  #  preprocess_prog is about 15 lines long and 
  # intersectWithSegmentAsLineResult is about 40 lines long
  attr_reader :x1, :y1, :x2, :y2
  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end

  def preprocess_prog
    if real_close(@x1, @x2) and real_close(@y1, @y2)
    then Point.new(@x1, @y1)
    else
        if @x1 > @x2
        then LineSegment.new(@x2, @y2, @x1, @y1)
        else
            if real_close(@x1, @x2) and (@y1 > @y2)
            then LineSegment.new(@x2, @y2, @x1, @y1)
            else LineSegment.new(@x1, @y1, @x2, @y2)
            end
        end
    end
  end

  def eval_prog env
    self
  end

  def shift(dx, dy)
   self.class.new(@x1 + dx, @y1 + dy, @x2 + dx, @y2 + dy)
  end 

  def intersect other
    other.intersectLineSegment(self.class.new(@x1, @y1, @x2, @y2))
  end 

  def intersectPoint p
    p.intersectLineSegment(self.class.new(@x1, @y1, @x2, @y2))
  end

  def intersectLine l
    l.intersectLineSegment(self.class.new(@x1, @y1, @x2, @y2))
  end

  def intersectVerticalLine vl
    vl.intersectLineSegment(self.class.new(@x1, @y1, @x2, @y2))
  end


  def intersectWithSegmentAsLineResult seg
    if (real_close(seg.x1, seg.x2)) 
      then                                          #vertical line
        segs = if (seg.y1 < @y1) then [seg, self] else [self, seg] end
        if (real_close(segs[0].y2, segs[1].y1)) 
          then Point.new(segs[0].x2, segs[0].y2)    #touching vl's
          else
            if (segs[0].y2 < segs[1].y1)
              then NoPoints.new                     #no overlap
              else                                  #overlapping
                if (segs[0].y2 > segs[1].y2)
                  then self.class.new(segs[1].x1, segs[1].y1, segs[1].x2, segs[1].y2)    #1 inside 0
                  else self.class.new(segs[1].x1, segs[1].y1, segs[0].x2, segs[0].y2)    #overlapping
                end
            end
        end
      else                                          #non-vertical line
        segs = if (seg.x1 < @x1) then [seg, self] else [self, seg] end
        if (real_close(segs[0].x2, segs[1].x1))
          then Pont.new(segs[0].x2, segs[0].y2)     #touching l's
          else
            if (segs[0].x2 < segs[1].x1)
              then NoPoints.new                     #no overlap
              else
                if (segs[0].x2 > segs[1].x2)
                  then self.class.new(segs[1].x1, segs[1].y1, segs[1].x2, segs[1].y2)   #1 inside 0
                  else self.class.new(segs[1].x1, segs[1].y1, segs[0].x2, segs[0].y2)   #overlapping
                end
            end
        end
    end
  end
end

# Note: there is no need for getter methods for the non-value classes

class Intersect < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end

  def preprocess_prog
    self.class.new(@e1, @e2)
  end

  def eval_prog env
   @eval_e1 = @e1.preprocess_prog.eval_prog env
   @eval_e2 = @e2.preprocess_prog.eval_prog env
   @eval_e2.intersect @eval_e1
  end

end

class Let < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: Look at Var to guide how you implement Let
  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end

  def preprocess_prog
    self.class.new(@s, @e1, @e2)
  end

  def eval_prog env
    eval_e1 = @e1.preprocess_prog.eval_prog(env)
    #updated_env = env.map(&:clone)
    @e2.preprocess_prog.eval_prog(env << [@s, eval_e1])
  end
end

class Var < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize s
    @s = s
  end
  def eval_prog env # remember: do not change this method
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end

  def preprocess_prog
    self
  end
end

class Shift < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end

  def preprocess_prog
    self.class.new(@dx, @dy, e)
  end

  def eval_prog env
    e.shift(@dx, @dy)
  end
end
