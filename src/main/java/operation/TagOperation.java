/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package operation;

import Operator.Identifier;
import Operator.Symbol;
import com.vesoft.nebula.client.graph.data.ResultSet;
import common.Operate;
import entity.Graph;
import entity.Schema;

import java.lang.reflect.Field;

public class TagOperation implements Operate<Schema> {
    private final Graph graph;

    public TagOperation(Graph graph ){
        this.graph = graph;
    }


    @Override
    public void create(Schema schema) throws NoSuchFieldException, IllegalAccessException {
            Class<? extends Schema> aClass = schema.getClass();
            Field name = aClass.getDeclaredField("name");
            name.setAccessible(true);
            System.out.println(Identifier.CREATE+Symbol.SPACE+ Identifier.TAG.toString()+Symbol.SPACE+name.get(schema)+Symbol.SPACE+ Symbol.LEFT_BRACKETS+ Symbol.RIGHT_BRACKETS);
            graph.run(Identifier.CREATE+Symbol.SPACE+ Identifier.TAG.toString()+Symbol.SPACE+name.get(schema)+Symbol.SPACE+ Symbol.LEFT_BRACKETS+ Symbol.RIGHT_BRACKETS);

    }

    @Override
    public void drop(Schema schema) throws NoSuchFieldException, IllegalAccessException {
        Class<? extends Schema> aClass = schema.getClass();
        Field name = aClass.getDeclaredField("name");
        name.setAccessible(true);
        System.out.println(Identifier.DROP+Symbol.SPACE+ Identifier.TAG.toString()+Symbol.SPACE+name.get(schema));
        graph.run(Identifier.DROP+Symbol.SPACE+ Identifier.TAG.toString() +Symbol.SPACE+name.get(schema));

    }

    public void update(Schema taG) {

    }

    public ResultSet showTags(){
        return  graph.run(Identifier.SHOW+Symbol.SPACE+ Identifier.TAGS.toString());
    }


    public ResultSet describeTag(Schema schema) throws NoSuchFieldException, IllegalAccessException {
        Class<? extends Schema> aClass = schema.getClass();
        Field name = aClass.getDeclaredField("name");
        //关闭JDK的语言访问检查
        name.setAccessible(true);
       System.out.println(Identifier.DESCRIBE+Symbol.SPACE+ Identifier.TAG.toString() +Symbol.SPACE+name.get(schema));
        return  graph.run(Identifier.DESCRIBE+Symbol.SPACE+ Identifier.TAG.toString() +Symbol.SPACE+name.get(schema));
    }



    public void create_Pro(Schema schema){
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
