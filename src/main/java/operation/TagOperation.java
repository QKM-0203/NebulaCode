package operation;

import Operator.Identifier;
import Operator.Symbol;
import com.vesoft.nebula.client.graph.data.ResultSet;
import common.Operate;
import entity.Graph;
import ogm.Property;
import ogm.Tag;

import java.lang.reflect.Field;
import java.util.List;

public class TagOperation implements Operate<Tag> {
    private final Graph graph;

    public TagOperation(Graph graph ){
        this.graph = graph;
    }


    @Override
    public void create(Tag tag) throws NoSuchFieldException, IllegalAccessException {
            Class<? extends Tag> aClass = tag.getClass();
            Field name = aClass.getDeclaredField("name");
            name.setAccessible(true);
            System.out.println(Identifier.CREATE+Symbol.SPACE+ Identifier.TAG.toString()+Symbol.SPACE+name.get(tag)+Symbol.SPACE+ Symbol.LEFT_BRACKETS+ Symbol.RIGHT_BRACKETS);
            graph.run(Identifier.CREATE+Symbol.SPACE+ Identifier.TAG.toString()+Symbol.SPACE+name.get(tag)+Symbol.SPACE+ Symbol.LEFT_BRACKETS+ Symbol.RIGHT_BRACKETS);

    }

    @Override
    public void drop(Tag tag) throws NoSuchFieldException, IllegalAccessException {
        Class<? extends Tag> aClass = tag.getClass();
        Field name = aClass.getDeclaredField("name");
        name.setAccessible(true);
        System.out.println(Identifier.DROP+Symbol.SPACE+ Identifier.TAG.toString()+Symbol.SPACE+name.get(tag));
        graph.run(Identifier.DROP+Symbol.SPACE+ Identifier.TAG.toString() +Symbol.SPACE+name.get(tag));

    }

    
    @Override
    public void update(Tag taG) {

    }

    public ResultSet showTags(){
        return  graph.run(Identifier.SHOW+Symbol.SPACE+ Identifier.TAGS.toString());
    }


    public ResultSet describeTag(Tag tag) throws NoSuchFieldException, IllegalAccessException {
        Class<? extends Tag> aClass = tag.getClass();
        Field name = aClass.getDeclaredField("name");
        //关闭JDK的语言访问检查
        name.setAccessible(true);
       System.out.println(Identifier.DESCRIBE+Symbol.SPACE+ Identifier.TAG.toString() +Symbol.SPACE+name.get(tag));
        return  graph.run(Identifier.DESCRIBE+Symbol.SPACE+ Identifier.TAG.toString() +Symbol.SPACE+name.get(tag));
    }



    public void create_Pro(Tag tag){
//        List<Property> propertyList = tag.getPropertyList();
//        StringBuilder stringBuilder = new StringBuilder(Identifier.CREATE+Symbol.SPACE+Identifier.TAG+Symbol.SPACE+
//                Symbol.LEFT_BRACKETS);
//        int i;
//        for (i = 0; i < propertyList.size()-1; i++) {
//            stringBuilder.append(propertyList.get(i).getProp_name()+Symbol.SPACE+propertyList.get(i).getData_type()+
//                    Symbol.COMMA+Symbol.SPACE);
//        }
//        stringBuilder.append(propertyList.get(i).)
    }


}
