require_relative 'Cloneable'
require_relative 'Roundable'
require_relative 'Transformable'

module IPC7351
    # mixin for calculating the boundaries of a geometric object
    #
    # the object must implement the loose_bounds() method, which must return
    # an array of Points that make up the boundaries of the object
    # this array is used by the Bounds class to calculate the actual boundaries
    # of an object
    # additionally, the object must include the Cloneable mixin
    module Boundable
        # Bounds bounds()
        # returns a Bounds object representing the boundaries of this object
        def bounds
            return Bounds.new(self)
        end

        # [Point] loose_bounds()
        # returns an array of Points that make up the boundaries of the object
        def loose_bounds()
            return []
        end
    end

    # represents the geometric boundaries (i.e. minimum and maximum x, y
    # coordinates of one or more objects
    class Bounds
        include Cloneable
        include Boundable 
        include Roundable 
        include Transformable
        attr_reader :min, :max

        # void initialize([Boundable object / Point point]...)
        def initialize(*args)
            x = []
            y = []
            args.each do |arg|
                case arg
                when Boundable
                    x += arg.loose_bounds.collect { |p| p.x }
                    y += arg.loose_bounds.collect { |p| p.y }
                when Point
                    x.push arg.x
                    y.push arg.y
                end
            end
            @min = Point.new(x.min, y.min)
            @max = Point.new(x.max, y.max)
        end

        # self.class expand!(dx, dy)
        # expands the existing boundaries by dx and dy
        def expand!(dx, dy)
            min = Point.new(@min.x - dx, @min.y - dy)
            max = Point.new(@max.x + dx, @max.y + dy)
            initialize(min, max)
            return self
        end

        # self.class expand(dx, dy = dx)
        # returns a new Bounds object with expanded boundaries
        # @sa expand!
        def expand(dx, dy = dx)
            return clone.expand!(dx, dy)
        end

        # self.class transform!(matrix)
        # applies the specified transformation matrix to the boundaries
        def transform!(matrix)
            bounds = [@min, @max].map { |bound| bound.transform(matrix) }
            initialize(*bounds)
            return self
        end

        # self.class round!(prec)
        # rounds the boundaries to the specified precision
        def round!(prec)
            bounds = [@min, @max].map { |bound| bound.round(prec) }
            initialize(*bounds)
            return self
        end

        # [Point] loose_bounds()
        # returns an array of Points representing the boundaries
        def loose_bounds
            return [@min, @max].collect { |p| Point.new(p) }
        end
    end
end

