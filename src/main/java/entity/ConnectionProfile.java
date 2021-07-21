package entity;


import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

public class ConnectionProfile  extends Graph{

    {
            //1.加载配置文件
            Properties pro = new Properties();
            //使用ClassLoader加载配置文件，获取字节输入流
            InputStream is = ConnectionProfile.class.getClassLoader().getResourceAsStream("nebula.properties");
            try {
               pro.load(is);
            } catch (IOException e) {
               e.printStackTrace();
            }
            //配置用户名和密码
            if(pro.getProperty("username") != null){
                super.username = pro.getProperty("username");
            }
            if(pro.getProperty("password") != null){
                super.password = pro.getProperty("password");
            }
            if(pro.getProperty("spaceName") != null){
                super.spaceName = pro.getProperty("spaceName");
            }
            //配置连接池
            if(pro.getProperty("maxConnSize") != null){
                super.nebulaPoolConfig.setMaxConnSize(Integer.parseInt(pro.getProperty("maxConnSize")));
            }
            if(pro.getProperty("minConnSize") != null){
                super.nebulaPoolConfig.setMinConnSize(Integer.parseInt(pro.getProperty("minConnSize")));
            }
            if(pro.getProperty("timeout") != null){
                super.nebulaPoolConfig.setTimeout(Integer.parseInt(pro.getProperty("timeout")));
            }
            if(pro.getProperty("idleTime") != null){
                super.nebulaPoolConfig.setIdleTime(Integer.parseInt(pro.getProperty("idleTime")));
            }
            if(pro.getProperty("port") != null){
                super.port = Integer.parseInt(pro.getProperty("port"));
            }
            if(pro.getProperty("host") != null){
                super.host = pro.getProperty("host");
            }
    }

    public ConnectionProfile(){

    }




}
