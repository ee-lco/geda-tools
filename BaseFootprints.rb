#encoding: UTF-8
require 'IPC7351'

module IPC7351
    module DualPads
        def generate_pads
            names = {
                "DIO"  => ["C", "A"],
            }[@type] || ["1", "2"]

            pf   = PadFactory.new(@env, @tols, @spec["D"], @spec["E"], @spec["L"])
            @pads.add(pf.pads(1, 1, 0, Geometry.side("left"),  [names[0]]))
            @pads.add(pf.pads(2, 1, 0, Geometry.side("right"), [names[1]]))
        end
    end

    module DualRowPads
        def generate_pads
            case @pins
            when 3
                pins         = 6
                names_bottom = ["1", nil, "2"]
                names_top    = [nil, "3", nil]
            when 5
                pins  = 6
                names_bottom = ["1", "2", "3"]
                names_top    = ["4", nil, "5"]
            else
                pins  = @pins
            end
            pf   = PadFactory.new(@env, @tols, @spec["E"], @spec["b"], @spec["L"])
            ppr  = pins / 2
            @pads.add(pf.pads(1,       ppr, @spec["e"].nom, Geometry.side("bottom"), names_bottom))
            @pads.add(pf.pads(ppr + 1, ppr, @spec["e"].nom, Geometry.side("top"),    names_top))
        end
    end

    module QuadRowPads
    end

    module Leaded
        def silkscreen
            lw      = @env["silkscreen.width"]
            clr     = @env["silkscreen.clearance"] + lw / 2.0
            outline = @settings["silkscreen.body"]

            min_line_length = lw * 4

            pads = pads_by_side
            dims = get_dims(@bl[outline], @bw[outline], 0, pads, clr, lw)

            lines = []
            Geometry.sides.each do |side|
                y2 =  dims[side]["y2"]
                y3 =  dims[side]["y3"]
                x1 =  dims[side.prev]["y2"]
                x4 = -dims[side.next]["y2"]

                if pads[side].empty?
                    lines.push(Drawable.line(x1, y2, x4, y2, lw).rotate_to(side))
                else
                    x2 = pads[side].bounds.expand(clr).rotate_from(side).min.x
                    x3 = pads[side].bounds.expand(clr).rotate_from(side).max.x
                    
                    if x2 - x1 >= min_line_length
                        lines.push(Drawable.line(x1, y2, x2, y2, lw).rotate_to(side))
                    else
                        x2 = x1
                    end
                    if x4 - x3 >= min_line_length
                        lines.push(Drawable.line(x3, y2, x4, y2, lw).rotate_to(side))
                    else
                        x3 = x4
                    end
                    if @mark && pads[side].include?(1)
                        lines.push(Drawable.line(x2, y2, x2, y3, lw).rotate_to(side)) if pads[side].sort.first.num == 1
                        lines.push(Drawable.line(x3, y2, x3, y3, lw).rotate_to(side)) if pads[side].sort.last.num  == 1
                    end
                end
            end

            add_layer("silkscreen", lines)

            return self
        end

    end

    module DualNoLead
        def silkscreen
            lw      = @env["silkscreen.width"]
            clr     = @env["silkscreen.clearance"] + lw / 2.0
            outline = @settings["silkscreen.body"]

            min_line_length = lw * 4

            pads = pads_by_side
            dims = get_dims(@bl[outline], @bw[outline], 0, pads, clr, lw)
            lines = []
            Geometry.sides.each do |side|
                if pads[side].empty?
                    y2 =  dims[side]["y2"]
                    x1 =  dims[side.prev]["y3"]
                    x2 =  dims[side.prev]["y1"]
                    x3 = -dims[side.prev]["y1"]
                    x4 = -dims[side.next]["y3"]

                    if @mark && pads[side.prev].first.num == 1
                        lines.push(Drawable.line(x1, y2, x3, y2, lw).rotate_to(side))
                    elsif @mark && pads[side.next].first.num == 1
                        lines.push(Drawable.line(x2, y2, x4, y2, lw).rotate_to(side))
                    else
                        lines.push(Drawable.line(x2, y2, x3, y2, lw).rotate_to(side))
                    end
                end
            end

            add_layer("silkscreen", lines)

            return self
        end
    end

    module DualRowNoLead
        def silkscreen
            lw      = @env["silkscreen.width"]
            clr     = @env["silkscreen.clearance"] + lw / 2.0
            outline = @settings["silkscreen.body"]

            min_line_length = lw * 4

            pads = pads_by_side(@pads)
            dims = get_dims(@bl[outline], @bw[outline], -lw / 2.0, pads, clr, lw)

            lines = []
            Geometry.sides.each do |side|
                y1 =  dims[side]["y1"]
                x1 =  dims[side.prev]["y1"]
                x4 = -dims[side.next]["y1"]

                if pads[side].empty?
                    lines.push(Drawable.line(x1, y1, x4, y1, lw).rotate_to(side))
                else
                    x2 = bounds[side].expand(clr).rotate_from(side).min.x
                    x3 = bounds[side].expand(clr).rotate_from(side).max.x
                    
                    if x2 - x1 >= min_line_length
                        unless pads[side].sort.first.num == 1
                            lines.push(Drawable.line(x1, y1, x2, y1, lw).rotate_to(side))
                        end
                    end
                    if x4 - x3 >= min_line_length
                        unless pads[side].sort.last.num == 1
                            lines.push(Drawable.line(x3, y1, x4, y1, lw).rotate_to(side))
                        end
                    end
                end
            end

            add_layer("silkscreen", lines)

            return self
        end

    end

    module QuadRowNoLead
        include DualRowNoLead
    end

    module RectEndCaps
        def select_env
            if @spec["D"].max < 1.6
                env = "Rectangular End Cap Length < 1.6 mm"
            else
                env = "Rectangular End Cap Length >= 1.6 mm"
            end

            return Environment.new(@settings["environment"], env)
        end
     end

    module GullWing
        def select_env
            srms = PadFactory.get_srms(@spec["E"], @spec["b"], @spec["L"])

            if srms["min"] > @bw["max"] || @spec["L"]["tol"] >= 0.5
                if @spec["e"]["nom"] <= 0.625
                    env = "Gull Wing Pitch <= 0.625 mm"
                else
                    env = "Gull Wing Pitch > 0.625 mm"
                end
            else
                if @spec["e"]["nom"] <= 0.625
                    env = "Outward L Lead Pitch <= 0.625 mm"
                else
                    env = "Outward L Lead Pitch > 0.625 mm"
                end
            end

            return Environment.new(@settings["environment"], env)
        end
     end
end

