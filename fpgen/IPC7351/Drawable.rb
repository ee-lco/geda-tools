require_relative 'Path'

module IPC7351
    module Drawable
        def self.line(*args)
            lw = args.pop
            return Polygon.new(Path.line(*args), lw)
        end

        def self.rectangle(*args)
            arg = args.pop
            case arg
            when Numeric
                lw = arg
            when Symbol
                style = arg
                lw    = args.pop
            else
                raise ArgumentError
            end

            if style == :fill
                ## @todo implement
                raise ArgumentError
                return Rectangle.new(*args, lw)
            else
                return Polygon.new(Path.rectangle(*args), lw)
            end
        end

        def self.polygon(*args)
            lw = args.pop
            return Polygon.new(Path.polygon(*args), lw)
        end

        def self.circle(*args)
            return Circle.new(*args)
        end
    end

    class Polygon < Path
        attr_reader :lw

        def initialize(*args)
            arg = args.pop
            case arg
            when Numeric
                @lw         = arg
                termination = :open
            when Symbol
                termination = arg
                @lw         = args.pop
            else
                raise ArgumentError
            end
            points = args.pop

            super(points, termination)
        end

        def transform(matrix)
            return Polygon.new(@points.map { |point| point.transform(matrix) }, @lw)
        end

        def round(placement)
            return Polygon.new(@points.map { |point| point.round(placement) }, @lw)
        end
    end

    class Circle
        include Transformable
        attr_reader :c, :x, :y, :r, :lw, :fill

        def initialize(*args)
            arg = args.pop
            case arg
            when Numeric
                @lw   = arg
                @fill = false
            when :fill
                @lw   = 0
                @fill = true
            else
                raise ArgumentError
            end
            case args.length
            when 2
                @c, @r = *args
            when 3
                x, y, @r = *args
                @c = Point.new(x, y)
            else
                raise ArgumentError
            end
        end

        def transform(matrix)
            return Circle.new(@c.transform(matrix), @r, @fill ? :fill : @lw)
        end

        def round(placement)
            return Circle.new(@c.round(placement), @r, @fill ? :fill : @lw)
        end
    end
end

