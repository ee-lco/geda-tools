module IPC7351
    # mixin for objects supporting geometric transformations using a transformation matrix
    #
    # the object must implement the transform! method
    module Transformable
        # self.class translate(Position pos, dx, dy = dx)
        # returns a new object translated dx, dy in the direction of pos
        def translate(pos, dx, dy = dx)
            return transform(pos.translation(dx, dy))
        end

        # self.class rotate(Position from_pos, Position to_pos)
        # returns a new object rotated from from_pos to to_pos
        def rotate(from_pos, to_pos)
            return transform(from_pos.rotation_to(to_pos))
        end

        # self.class rotate_to(to_pos)
        # returns a new object rotated from the default position to to_pos
        def rotate_to(to_pos)
            return rotate(Geometry::side("bottom"), to_pos)
        end

        # self.class rotate_from(from_pos)
        # returns a new object rotated from from_pos to the default position
        def rotate_from(from_pos)
            return rotate(from_pos, Geometry.side("bottom"))
        end

        # self.class transform(matrix)
        # returns a new object transformed by matrix
        def transform(matrix)
            return clone.transform!(matrix)
        end
    end
end

