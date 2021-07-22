package Operator;


public enum DateType {
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
    DateType(int length){
        this.length = length;
    }
    DateType(){

    }

    public int getLength() {
        return length;
    }

    public DateType setLength(int length) {
        this.length = length;
        return this;
    }
}
