package com.taobao.dpd.zfor.client;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public class zfor_client {

	private static final int MAX_BUF_LEN = 4096;
	private static final int DEFAULT_PORT = 1117;
	private static final int DEFAULT_UDP_TIMEOUT = 100;
	private static final int DEFAULT_MAX_HOSTADDRS = 16;
	private static final String ZFOR_CMD_DNS = "\000";
	private static final String DEFAULT_HOST = "127.0.0.1";
	private static final int MAXHOSTNAMELEN = 256;

	private static int port = DEFAULT_PORT;
	private static int timeout = DEFAULT_UDP_TIMEOUT;
	private static String host = DEFAULT_HOST;

	private static byte[] zfor_sync_call(byte[] req) throws Exception {
		DatagramSocket socket = new DatagramSocket();
		DatagramPacket ds = new DatagramPacket(req, req.length, InetAddress
				.getByName(host), port);
		socket.send(ds);

		socket.setSoTimeout(timeout);
		byte[] buf = new byte[MAX_BUF_LEN];
		ds = new DatagramPacket(buf, buf.length);
		socket.receive(ds);
		socket.close();
		return ds.getData();
	}

	/*
	 * 调用 ZFOR 服务解析给定的域名，若 ZFOR 服务调用未找到有效结果则返回系统自己的 gethostbyname 调用结果
	 * 
	 * @param virtual_host_name 待解析的域名字符串
	 * @parma fail_back Whether falling back to system-wide DNS resolving API when ZFOR failed.
	 */
	public static List<String> zfor_gethostbyname(String virtual_host_name, boolean fail_back)
		throws Exception {
		if (virtual_host_name == null
				|| virtual_host_name.equals("")
				|| virtual_host_name.length() > MAXHOSTNAMELEN) {
			throw new Exception("不允许解析空主机名或超长主机名");
		}
		List<String> ips = new LinkedList<String>();
		byte[] datas = null;
		try {
			String send_msg = (ZFOR_CMD_DNS + virtual_host_name);
			datas = zfor_sync_call(send_msg.getBytes());
		} catch (Exception e) {
			e.printStackTrace();
			if(fail_back) {
				// socket异常,试图使用系统的InetAddress.getByName解析
				ips.clear();// 清空ipinfos
				try {
					String hostname = gethostbyname(virtual_host_name);
					ips.add(hostname);
					return ips;
				} catch (Exception e1) {
					// InetAddress.getByName解析仍出现异常,无法处理异常抛出
					throw new Exception(
							"socket异常,使用系统的InetAddress.getByName解析,仍出现异常", e1);
				}
			} else {
				throw new Exception("Failed to resolving hostname through ZFOR", e);
			}
		}

		int total = (int) datas[0]; // 取出的ip数量

		// 若没有解析结果或数量不对则改用系统gethostbyname()接口解析
		if (total == 0 || datas[total * 4 + 1] != 0 || total > DEFAULT_MAX_HOSTADDRS) {
			if(fail_back) {
				ips.clear();// 清空ipinfos
				try {
					String hostname = gethostbyname(virtual_host_name);
					ips.add(hostname);
					return ips;// 直接返回处理结果
				} catch (Exception e1) {
					// InetAddress.getByName解析仍出现异常,无法处理异常抛出
					throw new Exception(
							"返回的解析数据没有结果或数量不对,使用系统的InetAddress.getByName解析,仍出现异常",
							e1);
				}
			} else {
				throw new Exception("Failed to resolving hostname through ZFOR");
			}
		}

		// 按照网络字节序获得IP地址
		for (int i = 0; i < total; i++) {
			long a = (int) (datas[4 * i + 1] & 0xff);
			int b = (int) (datas[4 * i + 2] & 0xff);
			int c = (int) (datas[4 * i + 3] & 0xff);
			int d = (int) (datas[4 * i + 4] & 0xff);

			String ip = a + "." + b + "." + c + "." + d;

			ips.add(ip);
		}
		return ips;
	}

	public static List<String> zfor_gethostbyname(String virtual_host_name)
		throws Exception {
		return zfor_gethostbyname(virtual_host_name, true);
	}

	public static String gethostbyname(String host_name)
		throws UnknownHostException {
		InetAddress addrinfo = InetAddress.getByName(host_name);
		return addrinfo.getHostAddress();
	}

	/*
	 * 设置请求 ZFOR 服务时使用的 UDP 端口号
	 * 
	 * @param 请求 ZFOR 服务时使用的 UDP 端口号，必须在 1~65535 范围内
	 */
	public static void zfor_set_udp_port(int p) {

		if (port < 0 || port > 65535) {
			// 无效的port使用默认值
			throw new RuntimeException("port值非法"+port);
		}

		port = p;
	}

	/*
	 * 设置请求 ZFOR 服务时的超时时间
	 * 
	 * @param ZFOR 请求超时时间（单位 ms），必须为非负整数
	 */
	public static void zfor_set_udp_timeout(int t) {

		if (timeout < 0) {
			// 无效的timeout
			throw new RuntimeException("timeout值无效"+t);
		} else {
			timeout = t;
		}
	}
}
