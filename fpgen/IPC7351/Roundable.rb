module IPC7351
    # mixin for objects whose position and/or size can be rounded
    #
    # the object must implement the round! method
    module Roundable
        # Numeric self.round(val, prec)
        # helper function to round a value to the specified precision
        def self.round(val, prec)
            return (val / prec).round * prec
        end

        # self.class round(precision)
        # self.class round(placement, size)
        # returns a new object with its position and size rounded to the specified
        # precision
        def round(*rounding)
            return clone.round!(*rounding)
        end
    end
end

