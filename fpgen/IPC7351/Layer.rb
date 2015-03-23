require 'forwardable'

module IPC7351
    class Layer
        include Enumerable
        extend Forwardable
        def_delegators :@elements, :[], :empty?, :include?, :length, :member?, :size
        attr_reader :name, :color

        def initialize(name, color, elements, *rounding)
            @name = name
            @color = color
            @elements = elements.map { |elt| elt.clone }
        end

        def each
            @elements.each { |elt| yield elt }
        end
    end
end

