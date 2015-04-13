module IPC7351
    module Pads
        module Dual
            def generate_pads
                names = {
                    "DIO"  => ["C", "A"],
                }[@type] || ["1", "2"]

                pwl  = @spec.include?("b")  ? @spec["b"]  : @spec["E"]
                pwr  = @spec.include?("b1") ? @spec["b1"] : pwl
                pfl  = PadFactory.new(@settings, @spec["D"], pwl, @spec["L"])
                pfr  = PadFactory.new(@settings, @spec["D"], pwr, @spec["L"], @spec["L1"])
                @pads.add(pfl.pads(1, 1, 0, Geometry.side("left"),  [names[0]]))
                @pads.add(pfr.pads(2, 1, 0, Geometry.side("right"), [names[1]]))
            end
        end

        module DualRow
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
                pf   = PadFactory.new(@settings, @spec["E"], @spec["b"], @spec["L"])
                ppr  = pins / 2
                @pads.add(pf.pads(1,       ppr, @spec["e"].nom, Geometry.side("bottom"), names_bottom))
                @pads.add(pf.pads(ppr + 1, ppr, @spec["e"].nom, Geometry.side("top"),    names_top))
            end
        end

        module QuadRow
            def generate_pads
                ppr  = @pins / 4
                pf_v = PadFactory.new(@settings, @spec["D"], @spec["b"], @spec["L"])
                pf_h = PadFactory.new(@settings, @spec["E"], @spec["b"], @spec["L"])
                @pads.add(pf_h.pads(1,           ppr, @spec["e"].nom, Geometry.side("bottom")))
                @pads.add(pf_v.pads(ppr     + 1, ppr, @spec["e"].nom, Geometry.side("right")))
                @pads.add(pf_h.pads(ppr * 2 + 1, ppr, @spec["e"].nom, Geometry.side("top")))
                @pads.add(pf_v.pads(ppr * 3 + 1, ppr, @spec["e"].nom, Geometry.side("left")))
            end
        end

        module SOT223
            def generate_pads
                pfb  = PadFactory.new(@settings, @spec["E"], @spec["b"],  @spec["L"])
                pft  = PadFactory.new(@settings, @spec["E"], @spec["b1"], @spec["L"])
                @pads.add(pfb.pads(1,     @pins - 1, @spec["e"].nom, Geometry.side("bottom")))
                @pads.add(pft.pads(@pins, 1,         @spec["e"].nom, Geometry.side("top")))
            end
        end
    end

    module Silkscreen
        module Base
            def pads_by_side
                pads = {}
                Geometry.sides.each do |side|
                    pads[side] = @pads.at_pos(side)
                end

                return pads
            end

            def get_dims(bl, bw, pads, clr, lw)
                dims = {}
                Geometry.sides.each do |side|
                    body = Geometry::Bounds.new(Path.rectangle(bl, bw)).rotate_from(side)

                    dims[side] = {}

                    dims[side]["pads_inner"]     = pads[side].bounds.rotate_from(side).min.y if !pads[side].empty?
                    dims[side]["pads_outer"]     = pads[side].bounds.rotate_from(side).max.y if !pads[side].empty?
                    dims[side]["pads_outer_clr"] = pads[side].bounds.rotate_from(side).max.y  - lw / 2.0 if !pads[side].empty?
                    cb = [body.max.y]
                    cb.push(pads[side.prev].bounds.expand(clr).rotate_from(side).max.y) if !pads[side.prev].empty?
                    cb.push(pads[side.next].bounds.expand(clr).rotate_from(side).max.y) if !pads[side.next].empty?
                    dims[side]["body_clr"] = cb.max
                end

                return dims
            end

            def mark_pin1(pad1, p1_y)
                size  = @settings["silkscreen.dot"]
                clr   = @settings["silkscreen.clearance"] + size / 2.0
                p1_y += clr

                p1_x = pad1.c.rotate_from(pad1.pos).x
                return [Drawable.circle(p1_x, p1_y, size / 2.0, :fill).rotate_to(pad1.pos)]
            end

            def silkscreen
                lw      = @settings["silkscreen.width"]
                clr     = @settings["silkscreen.clearance"] + lw / 2.0
                outline = @settings["silkscreen.body"]

                min_line_length = lw * 3

                pads = pads_by_side
                dims = get_dims(@bl[outline], @bw[outline], pads, clr, lw)

                elements = []
                Geometry.sides.each do |side|
                    elements.push(*silkscreen_side(side, pads, dims, lw, clr, min_line_length))
                end
                if @mark
                    elements.push(*mark_pin1(@pads[1], dims[@pads[1].pos]["pads_outer"]))
                end

                add_layer("silkscreen", elements)

                return self
            end 
        end

        module Leaded
            include Base

            def silkscreen_side(side, pads, dims, lw, clr, min_line_length)
                elements = []

                y      =  dims[side]["body_clr"]
                y_mark =  dims[side]["pads_outer_clr"]
                x1     = -dims[side.prev]["body_clr"]
                x2     =  dims[side.next]["body_clr"]

                if pads[side].empty?
                    elements.push(Drawable.line(x1, y, x2, y, lw).rotate_to(side))
                else
                    x1_mark = pads[side].bounds.expand(clr).rotate_from(side).min.x
                    x2_mark = pads[side].bounds.expand(clr).rotate_from(side).max.x
                    
                    if x1_mark - x1 >= min_line_length
                        elements.push(Drawable.line(x1, y, x1_mark, y, lw).rotate_to(side))
                    else
                        x1_mark = x1
                    end
                    if x2 - x2_mark >= min_line_length
                        elements.push(Drawable.line(x2_mark, y, x2, y, lw).rotate_to(side))
                    else
                        x2_mark = x2
                    end
                    if @mark && pads[side].include?(1)
                        elements.push(Drawable.line(x1_mark, y, x1_mark, y_mark, lw).rotate_to(side)) if pads[side].sort.first.num == 1
                        elements.push(Drawable.line(x2_mark, y, x2_mark, y_mark, lw).rotate_to(side)) if pads[side].sort.last.num  == 1
                    end
                end

                return elements
            end

        end

        module DualNoLead
            include Base

            def silkscreen_side(side, pads, dims, lw, clr, min_line_length)
                elements = []

                if pads[side].empty?
                    y       =  dims[side]["body_clr"]
                    x1_mark = -dims[side.prev]["pads_outer_clr"]
                    x1      = -dims[side.prev]["pads_inner"]
                    x2      =  dims[side.prev]["pads_inner"]
                    x2_mark =  dims[side.next]["pads_outer_clr"]

                    if @mark && pads[side.prev].first.num == 1
                        elements.push(Drawable.line(x1_mark, y, x2, y, lw).rotate_to(side))
                    elsif @mark && pads[side.next].first.num == 1
                        elements.push(Drawable.line(x1, y, x2_mark, y, lw).rotate_to(side))
                    else
                        elements.push(Drawable.line(x1, y, x2, y, lw).rotate_to(side))
                    end
                end

                return elements
            end
        end

        module DualRowNoLead
            include Base

            def silkscreen_side(side, pads, dims, lw, clr, min_line_length)
                elements = []

                y  =  dims[side]["pads_inner"]
                x1 =  dims[side.prev]["pads_inner"]
                x2 = -dims[side.next]["pads_inner"]

                if pads[side].empty?
                    elements.push(Drawable.line(x1, y, x2, y, lw).rotate_to(side))
                else
                    x1_mark = bounds[side].expand(clr).rotate_from(side).min.x
                    x2_mark = bounds[side].expand(clr).rotate_from(side).max.x
                    
                    if x1_mark - x1 >= min_line_length
                        unless pads[side].sort.first.num == 1
                            elements.push(Drawable.line(x1, y, x1_mark, y, lw).rotate_to(side))
                        end
                    end
                    if x2 - x2_mark >= min_line_length
                        unless pads[side].sort.last.num == 1
                            elements.push(Drawable.line(x2_mark, y, x2, y, lw).rotate_to(side))
                        end
                    end
                end

                return elements
            end

        end

        module QuadRowNoLead
            include DualRowNoLead
        end
    end

    module Leads
        module RectEndCap
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

                e = @spec.include?("e") ? @spec["e"]["nom"] : 1.0

                if srms["min"] > @bw["max"] || @spec["L"]["tol"] >= 0.5
                    if e <= 0.625
                        env = "Gull Wing Pitch <= 0.625 mm"
                    else
                        env = "Gull Wing Pitch > 0.625 mm"
                    end
                else
                    if e <= 0.625
                        env = "Outward L Lead Pitch <= 0.625 mm"
                    else
                        env = "Outward L Lead Pitch > 0.625 mm"
                    end
                end

                return Environment.new(@settings["environment"], env)
            end
         end

        module FlatLead
            def select_env
                return Environment.new(@settings["environment"], "Flat Lead")
            end
        end

        module InwardL
            def select_env
                return Environment.new(@settings["environment"], "Inward Flat Ribbon L", :inward)
            end
        end

        module UnderBodyOutwardL
            def select_env
                if @spec["A"].max <= 10
                    env = "Under-Body Outward L Height <= 10mm"
                else
                    env = "Under-Body Outward L Height > 10mm"
                end

                return Environment.new(@settings["environment"], env)
            end
        end
    end
end

