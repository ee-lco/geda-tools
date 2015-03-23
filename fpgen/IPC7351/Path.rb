require 'matrix'
require_relative 'Transformable'

module IPC7351
    class Point
        include Transformable
        attr_reader :x, :y

        def initialize(*args)
            case args.length
            when 1
                arg = args[0]
                case arg
                when Point
                    @x = arg.x
                    @y = arg.y
                when Matrix
                    @x = arg[0, 0]
                    @y = arg[1, 0]
                else
                    raise ArgumentError
                end
            when 2
                @x = args[0]
                @y = args[1]
            else
                raise ArgumentError
            end
        end

        def to_v
            return Matrix.column_vector([@x, @y, 1])
        end

        def to_m
            return Matrix.diagonal(@x, @y, 1)
        end

        def to_s
            return "(%f, %f)" % [@x, @y]
        end

        def transform(matrix)
            return Point.new(matrix * to_v)
        end

        def round(placement)
            return Point.new(Geometry.round(@x, placement),
                             Geometry.round(@y, placement))
        end
    end

    class Path
        include Enumerable
        extend Forwardable
        include Transformable
        def_delegators :@points, :[], :empty?, :include?, :length, :member?, :size

        def initialize(*args)
            case args.length
            when 1
                path = *args
                @points = path.map { |point| point }
            when 2
                points, termination = *args
                @points = points.map { |point| point.clone }
                if termination == :closed
                    @points.push @points.first
                end
            else
                raise ArgumentError
            end
        end

        def each
            @points.each { |point| yield point }
        end

        def self.line(*args)
            case args.length
            when 2
                p1, p2 = *args
            when 4
                x1, y1, x2, y2 = *args
                p1 = Point.new(x1, y1)
                p2 = Point.new(x2, y2)
            else
                raise ArgumentError
            end

            return Path.new([p1, p2], :open)
        end

        def self.rectangle(*args)
            case args.length
            when 2
                case args[0]
                when Point
                    p1, p2 = *args
                when Numeric
                    l, w = *args
                    p1 = Point.new(-l / 2.0, -w / 2.0)
                    p2 = Point.new( l / 2.0,  w / 2.0)
                else
                    raise ArgumentError
                end
            when 3
                c, l, w = *args
                p1 = Point.new(c.x - l / 2.0, c.y - w / 2.0)
                p2 = Point.new(c.x + l / 2.0, c.y + w / 2.0)
            else
                raise ArgumentError
            end

            return Path.new([Point.new(p1.x, p1.y), Point.new(p2.x, p1.y),
                             Point.new(p2.x, p2.y), Point.new(p1.x, p2.y)],
                             :closed)
        end

        def self.polygon(points)
            return Path.new(points, :closed)
        end

        def transform(matrix)
            return Path.new(@points.map { |point| point.transform(matrix) }, :open)
        end

        def bounds
            return Geometry::Bounds.new(@points)
        end

        def round(placement)
            return Path.new(@points.map { |point| point.round(placement) }, :open)
        end
    end
end
