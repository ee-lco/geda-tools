require 'matrix'
require_relative 'Cloneable'
require_relative 'Boundable'
require_relative 'Roundable'
require_relative 'Transformable'

module IPC7351
    class Point
        include Cloneable
        include Boundable
        include Roundable
        include Transformable
        attr_reader :x, :y

        # void initialize(Point p)
        # void initialize(Matrix m)
        # void initialize(Fixnum x, Fixnum y)
        def initialize(*args)
            case args[0]
            when Point
                @x = args[0].x
                @y = args[0].y
            when Matrix
                @x = args[0][0, 0]
                @y = args[0][1, 0]
            else
                @x = args[0].to_f
                @y = args[1].to_f
            end
        end

        # Matrix to_v()
        # returns the Point as a vector
        def to_v
            return Matrix.column_vector([@x, @y, 1])
        end

        # Matrix to_m()
        # returns the Point as a matrix
        def to_m
            return Matrix.diagonal(@x, @y, 1)
        end

        # String to_s()
        # returns a string representation of the Point
        def to_s
            return "(%f, %f)" % [@x, @y]
        end

        # Point transform!(Matrix matrix)
        # transforms the Point by multiplying its vector representation with a Matrix
        def transform!(matrix)
            m = matrix * to_v
            @x = m[0, 0]
            @y = m[1, 0]
            return self
        end

        # Point round(prec)
        # rounds the Point's x and y coordinates to the specified precision
        def round!(prec)
            @x = Roundable.round(@x, prec)
            @y = Roundable.round(@y, prec)
            return self
        end

        # [Point] loose_bounds()
        # returns an array with the Point
        def loose_bounds
            return [Point.new(self)]
        end
    end

    class Path
        include Enumerable
        extend Forwardable
        include Cloneable
        include Boundable
        include Roundable
        include Transformable
        def_delegators :@points, :[], :empty?, :include?, :length, :member?, :size
        attr_reader :lw

        # initialize(Point points...[], line_width, fill)
        def initialize(*args)
            if args.first.is_a?(Path)
                path = args.shift
                @points = *path.map { |point| Point.new(point) }
                @lw = path.lw
                @fill = path.fill?
            else
                @fill = args.pop
                @lw = args.pop.to_f
                @points = *args.map { |point| Point.new(point) }
            end
        end

        # boolean fill?()
        # yields all Points comprising the Path
        def each
            @points.each { |point| yield point }
        end

        # boolean fill?()
        # returns true if the Path is filled
        def fill?
            return @fill
        end

        # Path transform!(Matrix matrix)
        # transforms the Path by transforming its Points
        def transform!(matrix)
            @points = @points.map { |point| point.transform(matrix) }
            return self
        end

        # Path round!(prec)
        # rounds the Path's Points to the specified precision
        def round!(prec)
            @points = @points.map { |point| point.round(prec) }
            return self
        end

        # [Point] loose_bounds()
        # returns an array with all Points comprising the Path
        def loose_bounds
            return @points.collect { |point| Point.new(point) }
        end
    end

    class Circle
        include Cloneable
        include Boundable
        include Roundable
        include Transformable
        attr_reader :c, :r, :lw

        # void initialize(Point c, r, line_width, fill)
        def initialize(*args)
            if args.first.is_a?(Circle)
                circle = args.shift
                @c = circle.c
                @r = circle.r
                @lw = circle.lw
                @fill = circle.fill?
            else
                @c = Point.new(args.shift)
                @r = args.shift.to_f
                @lw = args.shift.to_f
                @fill = args.shift
            end
        end

        # boolean fill?()
        # returns true if the Circle is filled
        def fill?
            return @fill
        end

        # Circle transform!(Matrix matrix)
        # transforms the Circle by transforming its centre
        # @todo: scaling is not supported currently
        def transform!(matrix)
            @c = @c.transform(matrix)
            return self
        end

        # Circle round!(prec)
        # rounds the Circle's centre and radius the specified precision
        def round!(placement, size = placement)
            @c = @c.round(placement)
            @r = Roundable.round(@r, size)
            return self
        end

        # [Point] loose_bounds()
        # returns an array of Points forming the boundaries of the Circle
        def bounds
            return [Point.new(@c.x - @r, @c.y - @r), Point.new(@c.x + @r, @c.y + @r)]
        end
    end

    # module providing convenience functions to create various Shape objects
    module Shape
        # Path line(Point p1, Point p2, [line_width = 0])
        # Path line(x1, y1, x2, y2, [line_width = 0])
        def self.line(*args)
            if args.first.is_a?(Point)
                p1, p2 = args.shift(2)
            else
                x1, y1, x2, y2 = *args.shift(4).map { |coord| coord.to_f }
                p1 = Point.new(x1, y1)
                p2 = Point.new(x2, y2)
            end
            lw = (args.shift if args.first.respond_to?(:to_f)) || 0

            return Path.new(p1, p2, lw, false)
        end

        # Path rectangle([Point center], length, width, [line_width = 0], [:fill = false])
        # Path rectangle(Point p1, Point p2, [line_width = 0], [:fill = false])
        def self.rectangle(*args)
            c = args.shift if args.first.is_a?(Point)
            c ||= Point.new(0, 0)
            if args.first.is_a?(Point)
                p1 = c
                p2 = args.shift
            else
                l = args.shift.to_f
                w = args.shift.to_f

                p1 = Point.new(c.x - l / 2.0, c.y - w / 2.0)
                p2 = Point.new(c.x + l / 2.0, c.y + w / 2.0)
            end
            lw = (args.shift if args.first.respond_to?(:to_f)) || 0
            fill = ((args.shift == :fill) if args.first.is_a?(Symbol)) || false

            return Path.new(Point.new(p1.x, p1.y), Point.new(p2.x, p1.y),
                            Point.new(p2.x, p2.y), Point.new(p1.x, p2.y),
                            Point.new(p1.x, p1.y), lw, fill)
        end

        # Path polygon(Point points[], [line_width = 0], [:fill = false])
        def self.polygon(*args)
            points = []
            while args.first.is_a?(Point)
                points.push args.shift
            end
            points.push points.first

            lw = (args.shift if args.first.respond_to?(:to_f)) || 0
            fill = ((args.shift == :fill) if args.first.is_a?(Symbol)) || false

            return Path.new(*points, lw, fill)
        end

        # Circle circle(Point c, r, [line_width = 0], [:fill = false])
        # Circle circle(x, y, r, [line_width = 0], [:fill = false])
        def self.circle(*args)
            if args.first.is_a?(Point)
                c = args.shift
            else
                x = args.shift.to_f
                y = args.shift.to_f
                c = Point.new(x, y)
            end
            r = args.shift.to_f

            lw = (args.shift if args.first.respond_to?(:to_f)) || 0
            fill = ((args.shift == :fill) if args.first.is_a?(Symbol)) || false

            return Circle.new(c, r, lw, fill)
        end
    end
end
