using System;
using System.Collections.Generic;
using System.Linq;
using System.IO; 
using System.Text; 
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Routing;
using Microsoft.Extensions.DependencyInjection;

namespace Server
{
    public class Startup
    {
        // This method gets called by the runtime. Use this method to add services to the container.
        // For more information on how to configure your application, visit https://go.microsoft.com/fwlink/?LinkID=398940
        public void ConfigureServices(IServiceCollection services)
        {
            services.AddRouting(); 
        }
        public static void HandlePost(HttpContext context){
            var buf = new byte[(int)context.Request.ContentLength]; 
            context.Request.Body.Read(buf, 0, (int)context.Request.ContentLength); 
            string result = System.Text.Encoding.UTF8.GetString(buf); 
            System.Console.WriteLine(result);
            string previous = ""; 
            if (File.Exists("./data.json")) {
                previous = File.ReadAllText("./data.json");
            }
            using (FileStream fs = File.OpenWrite("./data.json")) {
                AddText(fs, previous + result + "\n"); 
            }
        }
        private static void AddText(FileStream fs, string value)
        {
            byte[] info = new UTF8Encoding(true).GetBytes(value);
            fs.Write(info, 0, info.Length);
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IHostingEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }

            var routeBuilder = new RouteBuilder(app);

            routeBuilder.MapPost(
                "data",
                (context) =>
                {
                    HandlePost(context);
                    return context.Response.WriteAsync($"ok");
                });

            var routes = routeBuilder.Build();
            app.UseRouter(routes);

            app.UseStaticFiles();

            app.Run(async (context) =>
            {
                await context.Response.WriteAsync("Hello World!");
            });
        }
    }
}
