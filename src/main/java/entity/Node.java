package entity;
import java.util.List;
import java.util.Map;


//创建多个标签
public class Node {

    //图空间中全局唯一vid
    private Object vid;


    //标签名字
    private String tagName;


    //Node的标签集合
    private List<Map<String, Object>> tagList;

    //创建一个节点，同时为该节点创建一个标签，
    public Node(Object vid,String tagName,Map<String, Object> tag) {
        this.vid = vid;
        this.tagName = tagName;
        this.tagList.add(0,tag);
    }


    //创建Node
    public boolean createNode(){
        return true;
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
