class MiniRuntime {
    public static void print(int i) { System.out.println(i); }
    public static void print(double d) { System.out.println(d); }
    public static void print(String s) { System.out.println(s); }

    public static String add(String s1, String s2) { return s1 + s2; }
    public static String add(int s1, String s2) { return s1 + s2; }
    public static String add(String s1, int s2) { return s1 + s2; }

    public static boolean lt(int i1, int i2) { return i1 <  i2; }
    public static boolean le(int i1, int i2) { return i1 <= i2; }
    public static boolean gt(int i1, int i2) { return i1 >  i2; }
    public static boolean ge(int i1, int i2) { return i1 >= i2; }
    public static boolean eq(int i1, int i2) { return i1 == i2; }
    public static boolean ne(int i1, int i2) { return i1 != i2; }

    public static boolean lt(double i1, double i2) { return i1 <  i2; }
    public static boolean le(double i1, double i2) { return i1 <= i2; }
    public static boolean gt(double i1, double i2) { return i1 >  i2; }
    public static boolean ge(double i1, double i2) { return i1 >= i2; }
    public static boolean eq(double i1, double i2) { return i1 == i2; }
    public static boolean ne(double i1, double i2) { return i1 != i2; }

    public static boolean lt(String i1, String i2) { return i1.compareTo(i2) <  0; }
    public static boolean le(String i1, String i2) { return i1.compareTo(i2) <= 0; }
    public static boolean gt(String i1, String i2) { return i1.compareTo(i2) >  0; }
    public static boolean ge(String i1, String i2) { return i1.compareTo(i2) >= 0; }
    public static boolean eq(String i1, String i2) { return i1.compareTo(i2) == 0; }
    public static boolean ne(String i1, String i2) { return i1.compareTo(i2) != 0; }
}

