module IPC7351
    # mixin for objects that can be cloned by passing self to new
    #
    # the object's initialize method must support a single argument of type
    # self.class
    module Cloneable
        # returns a clone of self
        def clone
            return self.class.new(self)
        end
    end
end

