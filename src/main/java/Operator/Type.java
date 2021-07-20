package Operator;


public enum Type {

//    public static final Map<Class,String> typeMap = new HashMap<>();
//    static{
//        //变长字符串
//        typeMap.put(StringBuffer.class,"string");
//        typeMap.put(Double.class,"double");
//        typeMap.put(Integer.class,"int");
//        typeMap.put(Boolean.class,"boolean");
//        typeMap.put(Date.class,"date");
//        typeMap.put(Time.class,"time");
//        typeMap.put(DateTime.class,"datetime");
//        typeMap.put(Timestamp.class,"timestamp");
//        //定长字符串
//        typeMap.put(String.class,"fixed_string");
//    }

    DATE,
    DATETIME,
    DOUBLE,
    FIXED_STRING(8),
    FLOAT,
    INT8,
    INT16,
    INT32,
    INT64,
    STRING,
    TIME,
    TIMESTAMP,
    BOOL;
    int length = 0;
    Type (int length){
        this.length = length;
    }
    Type(){

    }

    public int getLength() {
        return length;
    }

    public Type setLength(int length) {
        this.length = length;
        return this;
    }
}
