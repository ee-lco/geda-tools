require_relative 'Path'

module IPC7351
    class Drawable < Path
        attr_reader :lw

        def initialize(points, termination, lw)
            super(points, termination)
            @lw = lw
        end

        def self.line(*args)
            lw = args.pop
            return Drawable.new(Path.line(*args), :open, lw)
        end

        def self.rectangle(*args)
            lw = args.pop
            return Drawable.new(Path.rectangle(*args), :open, lw)
        end

        def self.polygon(*args)
            lw = args.pop
            return Drawable.new(Path.polygon(*args), :open, lw)
        end

        def transform(matrix)
            return Drawable.new(map { |point| point.transform(matrix) }, :open, @lw)
            ## @todo why doesn't this work?
            #return Drawable.new(super.transform(matrix), :open, @lw)
        end

        def round(placement)
            return Drawable.new(map { |point| point.round(placement) }, :open, @lw)
        end
    end
end

