require 'matrix'
require_relative 'Transformable'

module IPC7351
    module Geometry
        class Position
            attr_reader :name

            def initialize(name, translation)
                @name, @translation = name, translation
            end

            def ==(other)
                return to_s == other.to_s
            end

            def to_s
                return @name
            end

            def translation(dx = 1, dy = 1)
                return Matrix[[1, 0, @translation[0, 2] * dx], [0, 1, @translation[1, 2] * dy], [0, 0, 1]]
            end
        end

        Center = Position.new("center", Matrix[[0, 0, 0], [0, 0, 0], [0, 0, 1]])
        def center
            return Center
        end

        class PositionGroup < Array
        end

        class PositionGroupMember < Position
            attr_reader :prev, :next, :opposite

            def initialize(group, name, translation)
                super(name, translation)
                @group = group
                @group.push(self)
            end

            def rotation(rot_matrix, pos)
                rotation = Matrix.identity(3)
                while pos != self
                    rotation = rotation * rot_matrix
                    pos = pos.next
                end
                return rotation
            end

            def rotation_from(pos)
                rot_ccw = Matrix[[ 0, -1,  0], [ 1,  0,  0], [  0,  0,  1]]
                return rotation(rot_ccw, pos)
            end

            def rotation_to(pos)
                rot_cw = Matrix[[ 0,  1,  0], [-1,  0,  0], [  0,  0,  1]]
                return rotation(rot_cw, pos)
            end

            def index(name = @name)
                return @group.find_index { |pos| pos.name == name }
            end

            def prev
                return @group[(index - 1) % @group.length]
            end

            def next
                return @group[(index + 1) % @group.length]
            end

            def opposite
                return @group[(index + @group.length / 2) % @group.length]
            end

            def neighbors
                return [@prev, @next]
            end
        end

        Sides = PositionGroup.new
        class Side < PositionGroupMember
            attr_reader :per_axis, :par_axis

            def initialize(name, translation, per_axis, par_axis)
                super(Sides, name, translation)
                @per_axis, @par_axis = per_axis, par_axis
            end

            def self.instance(name)
                return Sides.find { |side| side.name == name }
            end
        end

        Side.new("bottom", Matrix[[ 1,  0,  0], [ 0,  1, -1], [ 0,  0,  1]], Matrix[[ 0, -1,  0]], Matrix[[ 1,  0,  0]])
        Side.new("right",  Matrix[[ 1,  0,  1], [ 0,  1,  0], [ 0,  0,  1]], Matrix[[ 1,  0,  0]], Matrix[[ 0, -1,  0]])
        Side.new("top",    Matrix[[ 1,  0,  0], [ 0,  1,  1], [ 0,  0,  1]], Matrix[[ 0,  1,  0]], Matrix[[-1,  0,  0]])
        Side.new("left",   Matrix[[ 1,  0, -1], [ 0,  1,  0], [ 0,  0,  1]], Matrix[[-1,  0,  0]], Matrix[[ 0,  1,  0]])
        def self.sides
            return Sides
        end
        def self.side(name)
            return Side.instance(name)
        end

        Corners = PositionGroup.new
        class Corner < PositionGroupMember
            attr_reader :sides

            def initialize(name, translation, side1, side2)
                super(Corners, name, translation)
                @sides = [side1, side2]
            end

            def self.instance(*args)
                case args.length
                when 1
                    name = args[0]
                    return Corners.find { |corner| corner.name == name }
                when 2
                    side1, side2 = *args
                    return Corners.find { |corner| corner.sides.include?(side1) && corner.sides.include?(side2) }
                end
            end
        end

        Corner.new("bottom-left",  Matrix[[ 1,  0, -1], [ 0,  1, -1], [ 0,  0,  1]], side("left"),   side("bottom"))
        Corner.new("bottom-right", Matrix[[ 1,  0,  1], [ 0,  1, -1], [ 0,  0,  1]], side("bottom"), side("right"))
        Corner.new("top-right",    Matrix[[ 1,  0,  1], [ 0,  1,  1], [ 0,  0,  1]], side("right"),  side("top"))
        Corner.new("top-left",     Matrix[[ 1,  0, -1], [ 0,  1,  1], [ 0,  0,  1]], side("top"),    side("left"))
        def self.corners
            return Corners
        end
        def self.corner(*args)
            return Corner.instance(*args)
        end

        class Bounds
            include Transformable
            attr_reader :min, :max

            def initialize(*args)
                x = []
                y = []
                args.flatten.each do |arg|
                    case arg
                    when Point
                        x.push(arg.x)
                        y.push(arg.y)
                    when Bounds
                        x.push(arg.min.x, arg.max.x)
                        y.push(arg.min.y, arg.max.y)
                    else
                        raise ArgumentError
                    end
                end
                @min = Point.new(x.min, y.min)
                @max = Point.new(x.max, y.max)
            end

            def expand(dx, dy = dx)
                return Bounds.new(Point.new(@min.x - dx, @min.y - dy), Point.new(@max.x + dx, @max.y + dy))
            end

            def transform(matrix)
                return Bounds.new(@min.transform(matrix), @max.transform(matrix))
            end
        end

        def self.round(val, prec)
            return (val / prec).round * prec
        end
    end
end

