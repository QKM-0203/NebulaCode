package entity;

import Operator.DateType;

public class Property {
    //property name.property names must be unique in each label
    private  String propName;

    private DateType dataDateType;

    private  boolean isNullable;

    private  Object defaultValue;


    public Property(String propName, DateType dataDateType, boolean isNullable, Object defaultValue) {
        this.propName = propName;
        this.dataDateType = dataDateType;
        this.isNullable = isNullable;
        this.defaultValue = defaultValue;
    }

    public String getPropName() {
        return propName;
    }

    public void setPropName(String propName) {
        this.propName = propName;
    }

    public DateType getDataType() {
        return dataDateType;
    }

    public void setDataType(DateType dataDateType) {
        this.dataDateType = dataDateType;
    }

    public boolean isNullable() {
        return isNullable;
    }

    public void setNullable(boolean nullable) {
        isNullable = nullable;
    }

    public Object getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(Object defaultValue) {
        this.defaultValue = defaultValue;
    }
}
