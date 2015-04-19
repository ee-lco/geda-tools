module IPC7351
    class Renderer
        def initialize(extension)
            @extension = extension
        end

        def render(footprint, io, layers)
            @io = io
            render_footprint(footprint, layers)
        end

        def save(*footprints)
            footprints.each { |fp| File.open(fp.filename + @extension, "w") { |file| render(fp, file) } }
        end
 
        def render_element(elt)
            case elt
            when Path
                render_path(elt)
            when Circle 
                render_circle(elt)
            when Pad
                render_pad(elt)
            else
                raise ArgumentError
            end
        end

        def render_layer(layer)
            layer.each { |elt| render_element(elt) }
        end

        def render_footprint(footprint, layers)
            layers = footprint.collect { |layer| layer.name } if layers.nil?
            footprint.select { |layer| layers.include?(layer.name) }.each { |layer| render_layer(layer) }
        end
    end
end

