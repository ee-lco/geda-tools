#encoding: UTF-8

module IPC7351
    class MinMax < Hash
        N = /([+-]?(?:\d+(?:\.\d*)?|\d*\.\d+))/

        def initialize(measurements)
            meas = measurements
            case meas
            when MinMax
                min = meas.min
                max = meas.max
            when String
                case meas
                when /#{N}\s*(?:~|±|\+\/?-)\s*#{N}/
                    # 2.00 ~ 0.20
                    nom  = $1.to_f
                    htol = $2.to_f.abs
                    min = nom - htol
                    max = nom + htol
                when /#{N}\s*-\s*#{N}\s*(?:\.\.)?\s*\+\s*#{N}/
                    # 2.00 -0.20..+0.20
                    nom  = $1.to_f
                    ntol = $2.to_f.abs
                    ptol = $3.to_f.abs
                    min = nom - ntol
                    max = nom + ptol
                when /#{N}\s*\.\.\s*#{N}/
                    # 1.80..2.20
                    min = $1.to_f
                    max = $2.to_f
                when /#{N}/
                    # 2.00
                    min = $1.to_f
                    max = $1.to_f
                else
                    raise ArgumentError
                end
            when Hash
                if meas.include?("min") && meas.include?("max")
                    min = meas["min"].to_f
                    max = meas["max"].to_f
                elsif meas.include?("nom") && meas.include?("tol")
                    min = meas["nom"].to_f - meas["tol"].to_f.abs / 2.0
                    max = meas["nom"].to_f + meas["tol"].to_f.abs / 2.0
                elsif meas.include?("nom") && meas.include?("htol")
                    min = meas["nom"].to_f - meas["htol"].to_f.abs
                    max = meas["nom"].to_f + meas["htol"].to_f.abs
                elsif meas.include?("nom") && meas.include?("-tol") && meas.include?("+tol")
                    min = meas["nom"].to_f - meas["-tol"].to_f.abs
                    max = meas["nom"].to_f + meas["+tol"].to_f.abs
                elsif meas.include?("min") && meas.include?("tol")
                    min = meas["min"].to_f
                    max = meas["min"].to_f + meas["tol"].to_f.abs
                elsif meas.include?("max") && meas.include?("tol")
                    min = meas["max"].to_f - meas["tol"].to_f.abs
                    max = meas["max"].to_f
                elsif meas.include?("min")
                    min = meas["min"].to_f
                    max = meas["min"].to_f
                elsif meas.include?("max")
                    min = meas["max"].to_f
                    max = meas["max"].to_f
                elsif meas.include?("nom")
                    min = meas["nom"].to_f
                    max = meas["nom"].to_f
                else
                    raise ArgumentError
                end
            else
                raise ArgumentError
            end

            self["min"] = min
            self["max"] = max
            self["nom"] = (min + max) / 2.0
            self["tol"] = max - min
        end

        def min
            return self["min"]
        end

        def max
            return self["max"]
        end

        def nom
            return (self["min"] + self["max"]) / 2.0
        end

        def tol
            return self["max"] - self["min"]
        end
    end
end

