package ogm;
import entity.ConnectionProfile;
import operation.TagOperation;

import java.util.HashMap;

public class Tag {
    private final String name;


    //增删属性、修改数据类型，也可以为属性设置、修改TTL（Time-To-Live）。
    private  HashMap<String,Object>  propertyHashMap;

    //指定时间戳差值，单位：秒。时间戳差值必须为64位非负整数。属性值和时间戳差值之和如果小于当前时间戳，
    // 属性就会过期。如果ttl_duration为0，属性永不过期。
    private float ttl_duration = 0;

    //指定为哪个属性设置存活时间，类型必须为int或者timestamp
    private String ttl_col;

    public float getTtl_duration() {
        return ttl_duration;
    }

    public void setTtl_duration(float ttl_duration) {
        this.ttl_duration = ttl_duration;
    }

    /**
     *
     * @param name  标签名
     * @param propertyHashMap  属性列表
     */
    public Tag(String name,HashMap<String,Object> propertyHashMap){
         this.name = name;
         this.propertyHashMap = propertyHashMap;
    }

    /**
     * 创建空属性的标签
     * @param name 标签名
     */
    public Tag(String name){
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public HashMap<String, Object> getPropertyHashMap() {
        return propertyHashMap;
    }


    public void creat_Tag() throws NoSuchFieldException, IllegalAccessException {
        TagOperation tagOperation = new TagOperation(new ConnectionProfile());
        tagOperation.create_Tag(this);
    }
}
