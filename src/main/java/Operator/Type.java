package Operator;


public enum Type {
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
