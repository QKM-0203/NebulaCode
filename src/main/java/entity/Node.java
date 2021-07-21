package entity;
import ogm.Tag;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;


public class Node {

    //图空间中全局唯一vid
    private final Object vid;

    //标签集合,仅检测标签的名称，不会检测具体属性。
    private Set<Tag> tagSet =  new HashSet<>();

    public Node(Object vid,Set<Tag> tagSet){
        this.tagSet = tagSet;
        this.vid = vid;
    }


    public Node(Object vid,Tag tagSet){
        this.tagSet.add(tagSet);
        this.vid = vid;
    }


    //打印出当前Node的信息
    public String getNode(){
        //利用vid
        return "";
    }

    //判断是否含有某个标签
    public boolean isHasTag(String tagName){
        //...
        return true;
    }

    //增加新的标签
    public boolean addTag(String tagName,Map<String, Object> tag){
        //...重新插入点带上新的标签
        return true;
    }

    //删除Node所有的标签,则该Node也就不存在了
    public boolean clearAllTags(){
        return true;
    }

    //更新Node节点某个标签的属性值
    public boolean updateTag(String tagName,Map<String, Object> tag){
        return true;
    }


    //....等等

}
