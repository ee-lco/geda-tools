module IPC7351
    module Transformable
        def translate(pos, x, y)
            return transform(pos.translation(x, y))
        end

        def rotate(from_pos, to_pos)
            return transform(from_pos.rotation_to(to_pos))
        end

        def rotate_to(to_pos)
            return rotate(Geometry::side("bottom"), to_pos)
        end

        def rotate_from(from_pos)
            return rotate(from_pos, Geometry.side("bottom"))
        end
    end
end

