package com.taobao.dpd.zfor.client.test;

import java.util.Iterator;
import java.util.List;

import com.taobao.dpd.zfor.client.zfor_client;

public class zfor_test {

	public static void main(String[] args) {

		try {
			if (args.length != 1) {
				System.out.println("useage: zfor_client_test <hostname>");
				return;
			}
			String virtual_host_name = args[0];
			List<String> ipinfos = zfor_client
					.zfor_gethostbyname(virtual_host_name);
			if (ipinfos.isEmpty()) {
				System.out.println(virtual_host_name + " can not be resolved");
				return;
			}
			Iterator<String> it = ipinfos.iterator();
			while (it.hasNext()) {
				String ip = (String) it.next();
				System.out.println(ip);
			}

		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	
}
