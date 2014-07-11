package in.dogue.antiqua.data

class AugInt(a:Int) {
    def %%(b:Int) = (a % b + b) % b
}
