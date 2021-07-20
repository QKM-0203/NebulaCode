package entity;
import java.util.List;

public class Tag {
    private final String name;

   //属性列表
    private List<Property> propertyList;


    //指定时间戳差值，单位：秒。时间戳差值必须为64位非负整数。属性值和时间戳差值之和如果小于当前时间戳，
    //属性就会过期。如果ttl_duration为0，属性永不过期。
    private long ttl_duration = 0;


    //指定为哪个属性设置存活时间，类型必须为int或者timestamp
    private String ttl_col;


    /**
     * 创建含有属性的标签
     * @param name 标签名称
     * @param propertyList  属性列表
     */
    public Tag(String name,  List<Property> propertyList){
        this.name = name;
        this.propertyList = propertyList;
    }

    /**
     * 创建空属性的标签
     * @param name 标签名
     */
    public Tag(String name){
        this.name = name;
    }


    public float getTtl_duration() {
        return ttl_duration;
    }

    public void setTtl_duration(long ttl_duration) {
        this.ttl_duration = ttl_duration;
    }


    public String getTtl_col() {
        return ttl_col;
    }

    public void setTtl_col(String ttl_col) {
        this.ttl_col = ttl_col;
    }


    public String getName() {
        return name;
    }


    public void setPropertyList(List<Property> propertyList) {
        this.propertyList = propertyList;
    }
}
