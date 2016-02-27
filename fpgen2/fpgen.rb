require 'forwardable'

# setting
#   name
#   component-type + dimensions => terminal
#   environment
#
#   chain
#     sot23 => smd => default
#     gull-wing => smd => default

$settings = {
  # units;Units;Millimeters,Micrometers,Inches,Mils
  #"units" => "mm",

  "component" =>
  {
  },

  "terminal" =>
  {
    "mask-dx"    => 0,
    "mask-dy"    => 0,
    "stencil-dx" => 0,
    "stencil-dy" => 0,

    "fabrication-tol"        => 0.050,
    "placement-tol"          => 0.025,
    "pad-pos-rounding"       => 0.01,
    "pad-size-rounding"      => 0.01,

    "courtyard-excess"       => 0.25,
    "courtyard-pos-rounding" => 0.01,

    "toe"       =>  0.35,
    "heel"      =>  0.35,
    "side"      =>  0.03,
    "periphery" =>  0.00,


    "hole-over-lead" => 0.20,
    "min-annular-ring" => 0.20,
    "pad-to-hole-ratio" => 1.50,
    "pad-to-slot-ratio" => 1.75,
    "thermal-id-over-hole" => 0.40,
    "min-thermal-od-over-id" => 0.30,
    "thermal-od-to-hole-ratio" => 1.10,
    "thermal-spoke-width" => 0.75,
  },
}

class PadStack
  extend Forwardable
  def_delegators :@layers, :[], :[]=

  def initialize
    @layers = {}
  end

  def name
    case @layers["top-pad"]["shape"]
    when :round
      name = "c"
    when :oblong
      name = "b"
    when :square
      name = "s"
    when :rectangle
      name = "r"
    when :d_shape
      name = "d"
    else
      name = "u"
    end

    x = @layers["top-pad"]["x"] || @layers["top-pad"]["d"]
    y = @layers["top-pad"]["y"] || @layers["top-pad"]["d"]
    name += size2str(x)

    unless x == y
      name += "_" + size2str(y)
    end

    ## @todo...

    name
  end

  def to_s
    '"%s":%s' % [name, @layers]
  end

private
  def size2str(size)
    "%d" % [ (size * 100).round ]
  end
end

class PadStackFactory
  attr_reader :settings

  def initialize(settings = nil)
    @settings = settings || $settings
  end

private
  def make_layer(shape, x, y)
    {
      "shape" => shape,
      "x" => x,
      "y" => y,
    }
  end

  def make_pad(shape, x, y)
    make_layer(
      shape,
      x,
      y,
    )
  end

  def make_mask(pad)
    make_layer(
      pad["shape"],
      pad["x"] + settings["terminal"]["mask-dx"],
      pad["y"] + settings["terminal"]["mask-dy"],
    )
  end

  def make_stencil(pad)
    make_layer(
      pad["shape"],
      pad["x"] + settings["terminal"]["stencil-dx"],
      pad["y"] + settings["terminal"]["stencil-dy"],
    )
  end

  def make_assembly(pad)
    make_layer(
      pad["shape"],
      pad["x"],
      pad["y"],
    )
  end
end

class SmdPadStackFactory < PadStackFactory
  def make_smd(shape, x, y = x)
    ps = PadStack.new

    pad = make_pad(shape, x, y)
    ps["top-pad"] = pad
    ps["top-mask"] = make_mask(pad)
    ps["top-stencil"] = make_stencil(pad)

    ## @todo should this really be part of the pad stack?
    ps["top-assembly"] = make_assembly(pad)

    ps
  end
end

class ThtPadStackFactory < PadStackFactory
  def make_tht(shape, x, y = x)
    ps = PadStack.new

    lead = make_lead(shape, x, y)
    hole = make_hole(lead)
    pad = make_pad(hole)
    thermal = make_thermal(hole)
    anti_pad = make_anti_pad(hole, thermal)

    ps["lead"] = lead
    ps["hole"] = hole
    ps["top-pad"] = ps["inner-pad"] = ps["bottom-pad"] = pad
    ps["top-mask"] = ps["bottom-mask"] = make_mask(pad)
    ps["thermal"] = thermal
    ps["anti-pad"] =anti_pad 

    ps
  end

private
  def make_assembly(pad)
    make_layer(
      pad["shape"],
      pad["x"],
      pad["y"],
    )
  end

  def make_lead(shape, x, y)
    make_layer(shape, x, y)
  end

  def lead_d(lead)
  end

  def make_hole(lead)
    # hole        = lead.d + hole-over-lead
    # hole (slot)
    #  w = min(lead.x, lead.y) + hole-over-lead
    #  l = max(lead.x, lead.y) + 2 * (w / 2)

    case lead["shape"]
    when :round
      lead_d = lead["x"]
    when :square, :rectangle
      lead_d = Math.sqrt(lead["x"] ** 2 + lead["y"] ** 2)
    end

    make_layer(
      lead["shape"],
      lead_d + settings["terminal"]["hole-over-lead"],
      lead_d + settings["terminal"]["hole-over-lead"],
    )
  end

  def make_pad(hole)
    # pad         = max(hole * pad-to-hole-ratio, hole + 2 * min-annular-ring)
    # pad (slot):
    #   w = min(hole.x, hole.y) * pad-to-slot-ratio, min(hole.x, hole.y) + 2 * min-annular-ring
    #   l = max(hole.x, hole.y) + (w - min(hole.x, hole.y))

    ratio = settings["terminal"]["pad-to-hole-ratio"]
    min = 2 * settings["terminal"]["min-annular-ring"]

    make_layer(
      hole["shape"],
      [hole["x"] * ratio, min].max,
      [hole["y"] * ratio, min].max,
    )
  end

  def make_thermal(hole)
    # thermal-id  = hole + thermal-id-over-hole
    # thermal-od  = hole * thermal-od-to-hole-ratio + thermal-id-over-hole + min-thermal-od-over-id
    # spoke-width = spoke-width-percentage * thermal-od / 4

    id_over = settings["terminal"]["thermal-id-over-hole"]
    od_ratio = settings["terminal"]["thermal-od-to-hole-ratio"]
    od_over = id_over + settings["terminal"]["min-thermal-od-over-id"]
    sw_ratio = settings["terminal"]["thermal-spoke-width"]
    sw_num = 4

    id = hole["x"] + id_over,
    od = hole["x"] * od_ratio + od_over
    sw = od * sw_ratio / sw_num.to_f
    {
      "id" => id,
      "od" => od,
      "spoke-width" => sw,
    }
  end

  def make_anti_pad(hole, thermal)
    # anti-pad    = thermal-od

    make_layer(
      hole["shape"],
      thermal["od"],
      thermal["od"],
    )
  end
end

class Tolerance
  attr_reader :min, :max

  def initialize(min, max)
    @min, @max = [min, max]
  end

  def nom
    (min + max) / 2.0
  end

  def tol
    (max - min)
  end
end

class IpcPadStackFactory < PadStackFactory
  def make_ipc(l, w, t)
    s = Tolerance.new(l.min - 2 * t.max, l.max - 2 * t.min)
puts("s_min: " + s.min.to_s)
puts("s_max: " + s.max.to_s)
puts("s_tol: " + s.tol.to_s)

    s_tol_rms = rss(l.tol, t.tol, t.tol)
#    s_max_rms = s.min + s_tol_rms (IPC-7351A)
    s_max_rms = s.nom + s_tol_rms / 2.0
puts("s_tol_rms: " + s_tol_rms.to_s)
puts("s_max_rms: " + s_max_rms.to_s)
    toe  = settings["terminal"]["toe"]#|periphery"]
    heel = settings["terminal"]["heel"]#|periphery"]
    side = settings["terminal"]["side"]#|periphery"]

    tf = 2 * settings["terminal"]["fabrication-tol"]
    tp = 2 * settings["terminal"]["placement-tol"]

    z_max = l.min + 2 * toe + rss(l.tol, tf, tp)
    g_min = s_max_rms - 2 * heel - rss(s_tol_rms, tf, tp)
    y_ref = w.min + 2 * side + rss(w.tol, tf, tp)
puts("z_max: " + z_max.to_s)
puts("g_min: " + g_min.to_s)
puts("y_ref: " + y_ref.to_s)

    rp = settings["terminal"]["pad-pos-rounding"]
    rs = settings["terminal"]["pad-size-rounding"]

#    x = round(y_ref, rs)
#    y = round((z_max - g_min) / 2.0, rs)
#    c = round((z_max + g_min) / 2.0, rp) / 2.0
    c = ((z_max + g_min) / 2.0) #/ 2.0
    x = (z_max - g_min) / 2.0
    y = y_ref
puts("c: " + c.to_s)
puts("x: " + x.to_s)
puts("y: " + y.to_s)
  end

private
  def rss(*vals)
    return Math.sqrt(vals.inject(0) {|sum, val| sum + val ** 2})
  end
end

class SVG
  #include REXML
  #include XRVG

  def render(ps)
    svg = ""

    
  end
end

spsf = SmdPadStackFactory.new
tpsf = ThtPadStackFactory.new
ipsf = IpcPadStackFactory.new
smd = spsf.make_smd(:rectangle, 2, 1)
tht = tpsf.make_tht(:round, 2)
ipc = ipsf.make_ipc(Tolerance.new(10, 10.65), Tolerance.new(0.33, 0.51), Tolerance.new(0.40, 1.27))

svg = SVG.new
puts svg.render(smd)
