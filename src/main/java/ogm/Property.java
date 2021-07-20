package ogm;

import Operator.Type;

public class Property {
    //属性名称。每个标签中的属性名称必须唯一。
    private  String prop_name;

    private  Type data_type;

    private  boolean isNullable;

    private  Object defaultValue;


    public Property(String prop_name, Type data_type, boolean isNullable, Object defaultValue) {
        this.prop_name = prop_name;
        this.data_type = data_type;
        this.isNullable = isNullable;
        this.defaultValue = defaultValue;
    }

    public String getProp_name() {
        return prop_name;
    }

    public void setProp_name(String prop_name) {
        this.prop_name = prop_name;
    }

    public Type getData_type() {
        return data_type;
    }

    public void setData_type(Type data_type) {
        this.data_type = data_type;
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
